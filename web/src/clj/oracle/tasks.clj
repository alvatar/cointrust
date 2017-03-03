(ns oracle.tasks
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [taoensso.carmine.message-queue :as mq]
            [cheshire.core :as json]
            [clj-time.core :as time]
            [clj-time.coerce :as time-coerce]
            ;; -----
            [oracle.utils :as utils]
            [oracle.redis :as redis]
            [oracle.database :as db]
            [oracle.events :as events]
            [oracle.common :as common]
            [oracle.redis :refer :all]
            [oracle.escrow :as escrow]
            [oracle.bitcoin :as bitcoin]
            [oracle.currency :as currency]))



;;
;; Utils
;;

(defn add-task-metadata [task-id data]
  (wcar* (r/set (str "task-metadata-" task-id) data)))

;;
;; Interface
;;

(defonce workers {:buy-requests-master {:qname "buy-requests-master-queue"
                                        :handler (ref nil)
                                        :worker (ref nil)}
                  :contracts-master {:qname "contracts-master-queue"
                                     :handler (ref nil)
                                     :worker (ref nil)}
                  :common-preemptive {:qname "common-preemptive-queue"
                                      :handler (ref nil)
                                      :worker (ref nil)}})

(defn initiate-buy-request [buyer-id amount currency-buyer currency-seller]
  (log/debug (format "Initiated buy request from buyer ID %d for %d %s" buyer-id amount currency-seller))
  (wcar* (mq/enqueue (get-in workers [:buy-requests-master :qname])
                     {:buyer-id buyer-id :amount amount
                      :currency-buyer currency-buyer :currency-seller currency-seller})))

(defn initiate-contract [buy-request]
  (if (:id buy-request)
    (log/debug "Initiated contract from buy request ID" (:id buy-request))
    (do
      (log/debugf "Buy request: %s" (with-out-str (pprint buy-request)))
      (throw (Exception. "The buy request doesn't have a seller ID"))))
  (when-not (:seller-id buy-request) (throw (Exception. "The buy request doesn't have an seller ID")))
  (wcar* (mq/enqueue (get-in workers [:contracts-master :qname])
                     buy-request
                     ;; Avoid the same buy request generating more than one contract
                     (str "contract-from-buy-request:" (:id buy-request)))))

(defn initiate-preemptive-task [tag data]
  (wcar* (mq/enqueue (get-in workers [:common-preemptive :qname])
                     {:tag tag :data data})))

;;
;; Matching
;;

;; This is the core of Cointrust. When a request to buy is received,
;; the matching engine will select a counterparty (seller), wait for
;; confirmation, and then create a contract and notify both parties.
;;
;; TODO: currently currency is ignored
;; TODO: transductors, optimize
;; TODO iterate ofer all friends^2 for their sell offer is extremely innefficient
(defn pick-counterparty [buyer-id buyer-specs]
  (let [blacklisted (mapv #(Long/parseLong %) (wcar* (r/smembers (str "buyer->blacklist:" buyer-id))))
        available (remove (fn [x] (some #(= % x) blacklisted)) (db/get-user-friends-of-friends buyer-id))
        offering (filter identity (map db/sell-offer-get-by-user available))
        offering-in-range (filter #(let [buyer-wants-amount (get-in buyer-specs [:wants :amount])
                                         buyer-wants-currency (get-in buyer-specs [:wants :currency])
                                         ;; In theory, we should check what the seller wants against what the buyer offers
                                         ;; Instead, we set the currency wanted by the seller as the one offered by the buyer
                                         ;; seller-wants-currency (get-in buyer-specs [:offers :currency])
                                         ]
                                     (pr-str %)
                                     (and (>= buyer-wants-amount
                                              (if (= (:currency %) buyer-wants-currency)
                                                (:min %)
                                                (currency/convert (:min %) (:currency %) buyer-wants-currency)))
                                          (<= buyer-wants-amount
                                              (if (= (:currency %) buyer-wants-currency)
                                                (:max %)
                                                (currency/convert (:max %) (:currency %) buyer-wants-currency)))))
                                  offering)]
    ;;(log/debug "PICK COUNTERPARTY, offering: " (pr-str offering))
    ;;(log/debug "PICK COUNTERPARTY, offering in range: " (pr-str offering-in-range))
    (:user (or (empty? offering-in-range) (rand-nth offering-in-range)))))

(defn blacklist-counterparty [buyer-id seller-id]
  (wcar* (r/sadd (str "buyer->blacklist:" buyer-id) seller-id)))

(defn clear-user-blacklist [buyer-id]
  (wcar* (r/del (str "buyer->blacklist:" buyer-id))))

;; This is kept in Redis, since it survives the buy request deletion in SQL
(defn set-buy-request-status [id status]
  (wcar* (r/hset "buy-request->status" id status)))

(defn get-buy-request-status [id]
  (wcar* (r/hget "buy-request->status" id)))

(defn clear-buy-request-status [id]
  (wcar* (r/hdel "buy-request->status" id)))

;;
;; Workers
;;

(defn idempotency-state-rebuild [uid]
  (let [ops-ttl (* 7 24 3600) ; 1 week
        op-key (str "idempotent-ops:" uid)
        now (utils/unix-now)
        [_ stored _] (wcar* (r/hsetnx op-key :global (json/generate-string {:created now :started now}))
                            (r/hgetall op-key)
                            (r/expire op-key ops-ttl))]
    (assoc-in (redis->json stored)
              [:global :now] now)))

(defn idempotency-state-set! [uid key new-map]
  (let [op-key (str "idempotent-ops:" uid)]
    ;;stored (some-> (wcar* (r/hget op-key key)) (json/parse-string true))
    (wcar* (r/hset op-key key (json/generate-string new-map)))
    new-map))

(defn idempotent-op* [uid tag state operation]
  (let [op-key (str "idempotent-ops:" uid)
        field (name tag)]
    (if-let [stored (tag state)]
      stored
      (let [op-result (or (operation) "\"<null>\"")]
        (wcar* (r/hset op-key field (try (json/generate-string op-result)
                                         (catch Exception e "\"<null>\""))))
        op-result))))

(defmacro idempotent-ops [uid tag state & body]
  `(idempotent-op* ~uid ~tag ~state (fn [] ~@body)))

(defn with-idempotent-transaction [uid tag state transaction]
  (let [op-key (str "idempotent-ops:" uid)
        field (name tag)]
    (if-let [stored (tag state)]
      stored
      (transaction
       (fn [result]
         (wcar* (r/hset op-key field (try (json/generate-string result)
                                          (catch Exception e "\"<null>\"")))))))))

;;
;; Master task queues: they track progress of an entity. They have time and state.
;;

;; Buy request master handler
;; 1. Pick counterparty (or multiple), and wait for response
;; counterparty (pick-counterparty buyer-id)
;; 2. If no response, pick another one (mark non-responding seller as blacklisted)
;; 3. When counterparty accepts, trigger contract creation
(defn buy-requests-master-handler
  [{:keys [qname mid message attempt] :as task}]
  (try
    (let [{:keys [buyer-id amount currency-buyer currency-seller]} message
          state (idempotency-state-rebuild mid)
          now (get-in state [:global :now])
          buy-request (with-idempotent-transaction mid :buy-request-create state
                        ;; TODO: exchange rate
                        #(if-let [result (db/buy-request-create! buyer-id amount currency-buyer currency-seller
                                                                 (get-in (oracle.currency/get-current-exchange-rates) [:rates :btc-usd])
                                                                 %)]
                           (do (log/debug "Buy request created:" result) result)
                           (throw (Exception. "Couldn't create buy-request"))))
          buy-request-id (:id buy-request)]
      ;; TODO: retrieve exchange rate (probably a background worker updating a Redis key)
      ;; (log/debugf "STATE: %s" state)
      ;; (log/debug "Processing buy request ID" buy-request-id)
      (idempotent-ops mid :event-buy-request-created state
                      (events/add-event! buyer-id :buy-request-created buy-request))
      (if-let [seller-id (with-idempotent-transaction mid :pick-counterparty state
                           #(when-let [counterparty (pick-counterparty
                                                     buyer-id
                                                     ;; Request to buy of what the seller has
                                                     ;; TODO: improve semantics upstream to match this
                                                     {:wants {:amount amount :currency currency-seller}
                                                      :offers {:currency currency-buyer}})]
                              (db/buy-request-set-seller! buy-request-id counterparty %)
                              (log/debugf "Buyer ID %s - Seller ID %s match for %s %s" buyer-id counterparty amount currency-seller)))]
        (do (idempotent-ops mid :event-sell-offer-matched state
                            (events/add-event! seller-id :sell-offer-matched buy-request)
                            (events/add-event! buyer-id :buy-request-matched {:id buy-request-id :seller-id seller-id}))
            ;; Here we check if its accepted. If so, the task succeeds. Handle timeout waiting for response.
            ;; (log/debugf "%s seconds have passed since this task was created." (float (/ (- now (:started (:global state))) 1000)))
            (let [buy-request-status (get-buy-request-status buy-request-id)]
              (cond
                ;; Buy request accepted
                (= buy-request-status "<accepted>")
                (do (clear-user-blacklist buyer-id)
                    {:status :success})
                ;; Buy request declined
                (or (= buy-request-status "<declined>")
                    (> now (utils/unix-after (time-coerce/to-date-time (:created buy-request)) (time/days 1))))
                (do (idempotency-state-set! mid :global (merge (:global state) {:started now}))
                    (idempotency-state-set! mid :pick-counterparty nil)
                    (idempotency-state-set! mid :event-sell-offer-matched nil)
                    (db/buy-request-unset-seller! buy-request-id)
                    (blacklist-counterparty buyer-id seller-id)
                    (clear-buy-request-status buy-request-id)
                    (if (= buy-request-status "<declined>")
                      (events/add-event! seller-id :buy-request-decline buy-request)
                      (events/add-event! seller-id :buy-request-timed-out buy-request))
                    {:status :retry :backoff-ms 1})
                :else
                {:status :retry :backoff-ms 1000})))
        (do ;; (log/debugf "No seller match (buy request ID %s). Retrying in 5 seconds." buy-request-id)
            {:status :retry :backoff-ms 5000})))
    (catch Exception e
      (let [prex (with-out-str (pprint e))]
        (log/debugf "Exception in task: %s" prex)
        (add-task-metadata mid {:queue "buy-requests" :exception prex :attempt attempt}))
      {:status :retry :backoff-ms 360000})))

;; Contract master handler
;; 1. Get current stage
;; 2. Check for conditions to change stage. If not met, retry later.
;; 3. Update SQL db: set new stage
;; 4. Notify/Set-aync-notification combo
;; 5. Set as stage "notified"
;;
;; Notes:
;; - Remember that events should be changed at the end, since a retry will go through a
;; different path afterwards, making idempotency irrelevant
(defn contracts-master-handler
  [{:keys [qname mid message attempt] :as all}]
  (try
    (let [{:keys [id] :as buy-request} message
          state (idempotency-state-rebuild mid)
          now (get-in state [:global :now])
          initial-contract (with-idempotent-transaction mid :contract state
                             #(if-let [contract (db/contract-create!
                                                 (merge buy-request {:input-address (bitcoin/wallet-get-fresh-address
                                                                                        (bitcoin/get-current-wallet))})
                                                 %)]
                                (do (log/debug "Contract created:" contract)
                                    contract)
                                (throw (Exception. "Couldn't create contract"))))
          contract-id (:id initial-contract)
          ;; Make sure we have the latest version of the contrast (forget idempotent op)
          contract (db/get-contract-by-id-with-last-event contract-id)]
      ;; (log/debugf "Attempt %d for contract ID %s. Contract: %s" attempt contract-id contract)
      ;; Task initialization
      ;; TODO: THIS WON'T ENSURE EXECUTION OF THESE COMMANDS
      (when (= attempt 1)
        ;; Keep in mind that we are deleting the request here, so we rely on the master task
        ;; to retrieve the buy request info from the idempotency cache in Redis
        (db/buy-request-delete! (:id buy-request)) ; Idempotent, must be done at the end
        (events/add-event! (:buyer-id contract) :contract-create contract)
        (events/add-event! (:seller-id contract) :contract-create contract)
        (escrow/setup-keys-for-contract! contract-id))
      (case (:stage contract)

        ;; Currently only used for testing
        "contract-success"
        {:status :success}

        ;; Contract can be cancelled anytime
        "contract-broken"
        (do (events/add-event! (:buyer-id contract) :contract-broken contract)
            (events/add-event! (:seller-id contract) :contract-broken contract)
            (db/contract-set-field! contract-id "escrow_open_for" (:seller-id contract))
            {:status :success})

        ;; We inform of the expected transaction and the freezing of price.
        ;; We wait for the seller to fund the escrow and provide the transfer details.
        "waiting-escrow"
        (cond (and (:transfer-info contract) (:escrow-funded contract))
              (do (with-idempotent-transaction mid :contract-add-event-waiting-transfer state
                    (fn [idemp]
                      (log/debug "Contract stage changed to \"waiting-transfer\"")
                      (db/contract-add-event! contract-id "waiting-transfer" nil idemp)))
                  (let [contract (merge contract {:stage "waiting-transfer"})]
                    (events/add-event! (:seller-id contract) :contract-escrow-funded contract)
                    (events/add-event! (:buyer-id contract) :contract-escrow-funded contract))
                  {:status :retry :backoff-ms 1})
              (> now (utils/unix-after (time-coerce/to-date-time (:created contract)) (time/minutes 20)))
              (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                    #(db/contract-add-event! contract-id "contract-broken" {:reason "escrow waiting period timed out"} %))
                  ;; Let the success contract stage handle it
                  {:status :retry :backoff-ms 1})
              :else
              {:status :retry :backoff-ms 1000})

        ;; The seller finalizes the contract when acknowledges reception of funds
        "waiting-transfer"
        (cond (:transfer-received contract)
              (do (db/contract-set-field! contract-id "escrow_open_for" (:buyer-id contract)) ; Idempotent
                  (events/add-event! (:buyer-id contract) :contract-success contract)
                  (events/add-event! (:seller-id contract) :contract-success contract)
                  (with-idempotent-transaction mid :contract-success state
                    #(db/contract-add-event! contract-id "contract-success" nil %))
                  ;; Let the success contract stage handle it
                  {:status :retry :backoff-ms 1})
              ;; The countdown
              (> now (utils/unix-after (time-coerce/to-date-time (:waiting-transfer-start contract)) (time/minutes 10)))
              (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                    #(db/contract-add-event! contract-id "contract-broken" {:reason "running contract timed out"} %))
                  {:status :success})
              :else
              {:status :retry :backoff-ms 1000})

        ;; Wrong stage, keep around for observation
        (do
          (log/debugf "Contract creation inconsistent: \n%s \n%s" (with-out-str (pprint contract))
                      (with-out-str (pprint buy-request)))
          {:status :retry :backoffs-ms 3600000})))
    (catch Exception e
      (let [prex (with-out-str (pprint e))]
        (log/debugf "Exception in task: %s" prex)
        (add-task-metadata mid {:queue "contracts" :exception prex :attempt attempt}))
      {:status :retry :backoff-ms 360000})))

;;
;; Preemptive task queues: They mark the accomplished micro-task, for the master
;; task to drive the progress.
;; Important: inside these tasks, only thing that *need to be performed ASAP* should
;; be placed. Everything else should go in the master tasks.
;;

(defmulti common-preemptive-handler
  (fn [{:keys [qname mid message attempt]}] (:tag message)))

;; TODO: secure all these calls

(defmethod common-preemptive-handler :buy-request/accept
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<accepted>") ; Idempotent
    (events/add-event! (:buyer-id buy-request) :buy-request-accept buy-request) ; Repeat OK
    ;; Add transfer info to buy-request before creating contract
    (initiate-contract (merge data buy-request)) ; Idempotent
    {:status :success}))

(defmethod common-preemptive-handler :buy-request/decline
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<declined>") ; Idempotent
    (events/add-event! (:buyer-id buy-request) :buy-request-decline buy-request)
    {:status :success}))

(defmethod common-preemptive-handler :contract/break
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    (db/contract-add-event! contract-id "contract-broken" nil)
    {:status :success}))

(defmethod common-preemptive-handler :contract/mark-transfer-received
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    (db/contract-set-field! contract-id "transfer_received" true)
    {:status :success}))

(defmethod common-preemptive-handler :escrow/release-to-user
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    ;; TEMPORARY APPROACH
    (if (bitcoin/wallet-send-coins (bitcoin/get-current-wallet)
          @bitcoin/current-app
          (:output-address contract)
          (long (* (:amount contract) 0.98)))
      (do 
        (db/contract-set-field! contract-id "escrow_released" true)
        (events/add-event! (:buyer-id contract) :contract-escrow-released contract) ; Allow repetition
        (events/add-event! (:seller-id contract) :contract-escrow-released contract))
      (do
        ;; HERE: HANDLE FAILURE
        (events/add-event! (:buyer-id contract) :contract-escrow-released contract) ; Allow repetition
        (events/add-event! (:seller-id contract) :contract-escrow-released contract))) ; Allow repetition
    {:status :success}))

;; (defmethod common-preemptive-handler :escrow/released
;;   [{:keys [?data ?reply-fn]}]
;;   (try (db/contract-set-field! (:id ?data) "released" true)
;;        (?reply-fn {:status :ok})
;;        (catch Exception e (pprint e) (?reply-fn {:status :error}))))

;;
;; Lifecyle
;;

(def worker-handlers {:buy-requests-master buy-requests-master-handler
                      :contracts-master contracts-master-handler
                      :common-preemptive common-preemptive-handler})

(defn- queue-status* [worker-id] (mq/queue-status redis-conn (get-in workers [worker-id :qname])))
(def buy-requests-status (partial queue-status* :buy-requests-master))
(def contracts-status (partial queue-status* :contracts-master))
(def preemptive-status (partial queue-status* :common-preemptive))
(defn queues-status [] (for [[id data] workers] (queue-status* id)))

(defn workers-stop! []
  (dosync
   (doseq [[id {:keys [worker]}] workers]
     (when @worker (mq/stop @worker) (ref-set worker nil)))))

(defn workers-start! []
  (dosync
   (doseq [[id {:keys [worker] :as worker-data}] workers]
     (alter worker
            #(or % (mq/worker redis-conn
                              (:qname worker-data)
                              {:eoq-backoff-ms 200
                               :throttle-ms 200
                               :handler (id worker-handlers)}))))))

(defn restart! [] (workers-stop!) (workers-start!))

;; (defonce start-workers_ (workers-start!))

;;
;; Testing utils
;;

;;
;; TODO: consider using System or something, take this out of here
;;

(defn populate-test-database! []
  (db/user-insert! "asdf" [])
  (db/user-insert! "ffff" ["asdf"])
  (db/user-insert! "aaaa" ["ffff"])
  (db/user-insert! "bbbb" ["asdf"])

  (db/user-insert! "cccc" [])

  (db/user-insert! "dddd" ["eeee"])
  (db/user-insert! "eeee" ["dddd"])
  'ok)

(defn total-wipeout!!! []
  (workers-stop!)
  ;;(bitcoin/system-reset!!!)
  (redis/flush!!!)
  (db/reset-database!!!)
  (populate-test-database!)
  ;;(bitcoin/system-start!)
  (workers-start!))


;;
;; Seller tests
;;

;; 1. Create a buy request
;; (initiate-buy-request 2 (oracle.common/currency-as-long 1.0 :btc) "usd" "btc")

;; 2. Accept buy request

;; 3. Seller sends money to Escrow
;; (oracle.database/contract-set-escrow-funded! 1)


;; -- EXTRA
;; Initialize Escrow
;; (oracle.escrow/set-seller-key 1 "fdsafdsa")
;; Buyer marks transfer sent
;; (oracle.tasks/initiate-preemptive-task :contract/mark-transfer-sent {:id 1})


;;
;; Buyer tests
;;

;; 1. Seller accepts buy request
;; IMPORTANT: if done outside the task, the seller will be reset to nil if that was the value of the imdepotency storage
#_(do
  (oracle.database/buy-request-set-seller! 1 2)
  (oracle.tasks/initiate-preemptive-task :buy-request/accept
                                         {:id 1
                                          :transfer-info "transfer info"}))

;; 2. Escrow initialization
;; (oracle.database/contract-set-escrow-funded! 1)

;; 3. Seller marks transfer received
;; (oracle.tasks/initiate-preemptive-task :contract/mark-transfer-received {:id 1})


;; -- EXTRA

;; Seller declines buy request
;; (oracle.tasks/initiate-preemptive-task :buy-request/decline {:id 1})

;; Directly create contract
#_(oracle.tasks/initiate-contract {:id 1, :created #inst "2017-01-16T18:22:07.389569000-00:00", :buyer-id 1, :seller-id 2, :amount 100000000, :currency-buyer "usd", :currency-seller "btc", :exchange-rate 1000.000000M, :transfer-info "Hakuna Matata Bank.
Publius Cornelius Scipio
Ibiza, Spain.
IBAN 12341234123431234
SWIFT YUPYUP12"})

(defn contract-force-success [id]
  (let [contract (db/get-contract-by-id id)]
    (db/contract-add-event! id "contract-success" nil)
    (events/add-event! (:seller-id contract) :contract-success contract)
    (events/add-event! (:buyer-id contract) :contract-success contract)))
