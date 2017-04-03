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
  (log/debugf "Initiated contract from buy request ID %s" (:id buy-request))
  (when-not (and (:id buy-request) (:seller-id buy-request))
    (log/debugf "Buy request: %s" (with-out-str (pprint buy-request)))
    (throw (Exception. "The buy request doesn't have all required data")))
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
;; TODO: transductors, optimize
;; TODO iterate ofer all friends^2 for their sell offer is extremely innefficient
(defn pick-counterparty [buy-request buyer-specs]
  (let [buyer-id (:buyer-id buy-request)
        blacklisted (mapv #(Long/parseLong %) (wcar* (r/smembers (str "buyer->blacklist:" buyer-id))))
        friends2 (map :id (db/get-user-friends-of-friends buyer-id))
        available (remove (fn [x] (some #(= % x) blacklisted)) friends2)
        exchange-rates (currency/get-current-exchange-rates)
        offering (filter identity (map db/get-sell-offer-by-user available))
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
                                                (currency/convert-as-long (:min %) (:currency %) buyer-wants-currency exchange-rates)))
                                          (<= buyer-wants-amount
                                              (if (= (:currency %) buyer-wants-currency)
                                                (:max %)
                                                (currency/convert-as-long (:max %) (:currency %) buyer-wants-currency exchange-rates)))))
                                  offering)]
    (when-not (empty? offering-in-range)
      (let [offer (rand-nth offering-in-range)]
        ;; Freeze exchange rate if matched, and set the premium of the offer
        (db/buy-request-update! (:id buy-request) {:exchange_rate (get-in exchange-rates [:rates :btc-usd])
                                                   :premium (:premium offer)})
        (:user offer)))))

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
      ;; (log/debugf "STATE: %s" state)
      ;; (log/debug "Processing buy request: " buy-request)
      (idempotent-ops mid :event-buy-request-created state
                      (events/send-event! buyer-id :buy-request/create buy-request))
      (if-let [seller-id (with-idempotent-transaction mid :pick-counterparty state
                           #(when-let [counterparty (pick-counterparty
                                                     buy-request
                                                     ;; Request to buy of what the seller has
                                                     {:wants {:amount amount :currency currency-seller}
                                                      :offers {:currency currency-buyer}})]
                              (db/buy-request-set-seller! buy-request-id counterparty %)
                              (log/debugf "Buyer ID %s - Seller ID %s match for %s %s" buyer-id counterparty (common/currency-as-float amount currency-seller) currency-seller)))]
        (let [full-buy-request (db/get-buy-request-by-id buy-request-id)]
          (idempotent-ops mid :event-sell-offer-matched state
                          (events/send-event! seller-id :sell-offer-match/create full-buy-request)
                          (events/send-event! buyer-id :buy-request/update full-buy-request))
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
                    ;; TEMP do not blacklist
                    ;; (blacklist-counterparty buyer-id seller-id)
                    (clear-buy-request-status buy-request-id)
                    (events/send-event! seller-id :buy-request/update (dissoc buy-request :seller-id))
                    {:status :retry :backoff-ms 1})
                :else
                {:status :retry :backoff-ms 1000})))
        (do ;; (log/debugf "No seller match (buy request ID %s). Retrying in 5 seconds." buy-request-id)
            {:status :retry :backoff-ms 1000})))
    (catch Exception e
      (let [prex (with-out-str (pprint e))]
        (log/errorf "Exception in task: %s" prex)
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
                                                                                        (bitcoin/get-current-wallet))
                                                                     :fee 100})
                                                 %)]
                                (do (log/debug "Contract created:" contract)
                                    contract)
                                (throw (Exception. "Couldn't create contract"))))
          contract-id (:id initial-contract)
          ;; Make sure we have the latest version of the contrast (forget idempotent op)
          contract (db/get-contract-by-id contract-id)]
      ;; (log/debugf "Attempt %d for contract ID %s. Contract: %s" attempt contract-id contract)
      ;; Task initialization
      ;; TODO: THIS WON'T ENSURE EXECUTION OF THESE COMMANDS
      (idempotent-ops mid :initialization state
                      ;; Keep in mind that we are deleting the request here, so we rely on the master task
                      ;; to retrieve the buy request info from the idempotency cache in Redis
                      (db/buy-request-delete! (:id buy-request)) ; Idempotent, must be done at the end
                      (events/send-event! (:buyer-id contract) :contract/create contract)
                      (events/send-event! (:seller-id contract) :contract/create contract)
                      (escrow/setup-keys-for-contract! contract-id))
      (case (:stage contract)

        ;; Currently only used for testing
        "contract-success"
        {:status :success}

        ;; Contract can be cancelled anytime
        "contract-broken"
        (do (events/send-event! (:buyer-id contract) :contract/update contract)
            (events/send-event! (:seller-id contract) :contract/update contract)
            (db/contract-update! contract-id {:escrow_open_for (:seller-id contract)})
            {:status :success})

        ;; Contract can be cancelled anytime
        "contract-broken/escrow-insufficient"
        (do (events/send-event! (:buyer-id contract) :contract/update contract)
            (events/send-event! (:seller-id contract) :contract/update contract)
            (db/contract-update! contract-id {:escrow_open_for (:seller-id contract)})
            {:status :success})

        "waiting-start"
        (cond (:started-timestamp contract)
              ;; Contract marked as started
              (do (with-idempotent-transaction mid :contract-add-event-waiting-start state
                    (fn [idemp]
                      (log/debug "Contract stage changed to \"waiting-escrow\"")
                      (db/contract-add-event! contract-id "waiting-escrow" nil idemp)))
                  (let [contract (merge contract {:stage "waiting-escrow"})]
                    (events/send-event! (:seller-id contract) :contract/update contract)
                    (events/send-event! (:buyer-id contract) :contract/update contract))
                  {:status :retry :backoff-ms 1})
              (> now (utils/unix-after (time-coerce/to-date-time (:created contract)) (time/hours 12)))
              (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                    #(db/contract-add-event! contract-id "contract-broken" {:reason "contract start waiting period timed out"} %))
                  ;; Let the success contract stage handle it
                  {:status :retry :backoff-ms 1})
              :else
              {:status :retry :backoff-ms 1000})

        ;; We inform of the expected transaction and the freezing of price.
        ;; We wait for the seller to fund the escrow and provide the transfer details.
        "waiting-escrow"
        (cond (and (:transfer-info contract) (:escrow-funded-timestamp contract))
              ;; Amount expected: amount - premium
              (if (>= (:escrow-amount contract) (* (:amount contract) (common/long->decr (:premium contract))))
                ;; Received the right amount
                (do (with-idempotent-transaction mid :contract-add-event-waiting-transfer state
                      (fn [idemp]
                        (log/debug "Contract stage changed to \"waiting-transfer\"")
                        (db/contract-add-event! contract-id "waiting-transfer" nil idemp)))
                    (let [contract (merge contract {:stage "waiting-transfer"})]
                      (events/send-event! (:seller-id contract) :contract/update contract)
                      (events/send-event! (:buyer-id contract) :contract/update contract))
                    {:status :retry :backoff-ms 1})
                ;; Received less than required
                (do (with-idempotent-transaction mid :contract-add-event-escrow-insufficient state
                      (fn [idemp]
                        (log/debug "Contract stage changed to \"contract-broken/escrow-insufficient\"")
                        (db/contract-add-event! contract-id "contract-broken/escrow-insufficient" nil idemp)))
                    (let [contract (merge contract {:stage "contract-broken/escrow-insufficient"})]
                      (events/send-event! (:seller-id contract) :contract/update contract)
                      (events/send-event! (:buyer-id contract) :contract/update contract))
                    {:status :retry :backoff-ms 1}))
              ;; The countdown
              (> now (utils/unix-after (time-coerce/to-date-time (:started-timestamp contract)) (time/minutes 60)))
              (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                    #(db/contract-add-event! contract-id "contract-broken" {:reason "escrow waiting period timed out"} %))
                  ;; Let the success contract stage handle it
                  {:status :retry :backoff-ms 1})
              :else
              {:status :retry :backoff-ms 1000})

        ;; The seller finalizes the contract when acknowledges reception of funds
        "waiting-transfer"
        (cond (and (:transfer-received contract)
                   (:escrow-seller-has-key contract)
                   (:escrow-buyer-has-key contract))
              (let [contract (merge contract {:stage "contract-success"})]
                (db/contract-update! contract-id {:escrow_open_for (:buyer-id contract)}) ; Idempotent
                (events/send-event! (:buyer-id contract) :contract/update contract)
                (events/send-event! (:seller-id contract) :contract/update contract)
                (with-idempotent-transaction mid :contract-success state
                  #(db/contract-add-event! contract-id "contract-success" nil %))
                ;; Let the success contract stage handle it
                {:status :retry :backoff-ms 1})
              ;; The countdown
              (> now (utils/unix-after (time-coerce/to-date-time (:escrow-funded-timestamp contract)) (time/minutes 30)))
              (let [contract (merge contract {:stage "contract-broken"})]
                (events/send-event! (:buyer-id contract) :contract/update contract)
                (events/send-event! (:seller-id contract) :contract/update contract)
                (with-idempotent-transaction mid :contract-add-event-contract-boken state
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
    (if buy-request
      (do (log/debug message)
          (set-buy-request-status buy-request-id "<accepted>") ; Idempotent
          (events/send-event! (:buyer-id buy-request) :buy-request/delete buy-request) ; Repeat OK
          ;; Add transfer info to buy-request before creating contract
          (initiate-contract (merge data buy-request)) ; Idempotent
          {:status :success})
      (do
        (log/errorf "BUY REQUEST ID %s NOT FOUND!" buy-request-id)
        {:status :success}))))

(defmethod common-preemptive-handler :buy-request/decline
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<declined>") ; Idempotent
    (events/send-event! (:buyer-id buy-request) :buy-request/update (dissoc buy-request :seller-id))
    {:status :success}))

(defmethod common-preemptive-handler :escrow/release-to-user
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    ;; TEMPORARY APPROACH
    (if (:escrow-open-for contract)
      (if (bitcoin/wallet-send-coins (bitcoin/get-current-wallet)
            @bitcoin/current-app
            contract-id
            (:output-address contract)
            (if (= (:escrow-open-for contract) (:buyer-id contract))
              ;; Substract the Cointrust fee (applying also the premium)
              (long (* (:amount contract)
                       (common/long->decr (:fee contract))
                       (common/long->decr (:premium contract))))
              ;; Subtract enough for the miners fee
              (- (long (* (:amount contract)
                          (common/long->decr (:premium contract))))
                 (if (= (env :env) "production")
                   70000 ; http://bitcoinexchangerate.org/fees
                   100000))))
        (let [contract (merge contract {:escrow-release "<success>"})]
          (db/contract-update! contract-id {:escrow_release "<success>"})
          (events/send-event! (:buyer-id contract) :contract/update contract)
          (events/send-event! (:seller-id contract) :contract/update contract)
          (db/log! "info" "bitcoin" {:operation "escrow-release" :result "success"}))
        (let [contract (merge contract {:escrow-release "<failure>"})]
          (db/contract-update! contract-id {:escrow_release "<failure>"})
          (events/send-event! (:buyer-id contract) :contract/update contract)
          (events/send-event! (:seller-id contract) :contract/update contract)
          (db/log! "info" "bitcoin" {:operation "escrow-release" :result "failure"})))
      (log/errorf "Error releasing to user -- Not open for any party. Contract: %s" (with-out-str (pprint contract))))
    {:status :success}))

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
  (doseq [u common/fake-users]
    (let [{:keys [fb-id user-name user-id user-hash friend-hashes]} u]
      (println u)
      (db/user-insert! fb-id user-name user-hash friend-hashes)))

  ;; (db/user-insert! 145228535996960 "John" "asdf" [])
  ;; (db/user-insert! 145228535996960 "Ada" "ffff" ["asdf"])
  ;; (db/user-insert! 10100642548250434 "Scipio" "aaaa" ["ffff"])
  ;; (db/user-insert! 10213129106885586 "Thor" "bbbb" ["asdf"])

  ;; (db/user-insert! 145228535996960 "Julius" "cccc" [])
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
;; (oracle.database/contract-set-escrow-funded! 1 (oracle.common/btc->satoshi 2) "fake-transaction-hash")


;; -- EXTRA
;; Initialize Escrow
;; (oracle.escrow/set-seller-key 1 "fdsafdsa")
;; Buyer marks transfer sent
;; (oracle.tasks/initiate-preemptive-task :contract/mark-transfer-sent {:id 1})


;;
;; Buyer tests
;;

;; 1. Set offer to be matched
;; (oracle.database/sell-offer-set! 2 "usd" 100 300000 100)

;; 2. Accept buy request
;; (oracle.tasks/initiate-preemptive-task :buy-request/accept {:id 1 :transfer-info "transfer info"})

;; 3. Escrow initialization
;; (oracle.database/contract-set-escrow-funded! 1 (oracle.common/btc->satoshi 2) "fake-transaction-hash")

;; 4. Seller marks transfer received
;; (oracle.tasks/initiate-preemptive-task :contract/mark-transfer-received {:id 1})


;; -- EXTRA

;; Seller declines buy request
;; (oracle.tasks/initiate-preemptive-task :buy-request/decline {:id 1})

;; Directly create contract
;; (oracle.tasks/initiate-contract {:id 1, :created #inst "2017-01-16T18:22:07.389569000-00:00", :buyer-id 1, :seller-id 2, :amount 100000000, :currency-buyer "usd", :currency-seller "btc", :exchange-rate 1000.000000M, :transfer-info "cosmicbro" :premium 100 :fee 100})

(defn contract-force-success [id]
  (let [contract (db/get-contract-by-id-fast id)
        contract (merge contract {:stage "contract-success"})]
    (db/contract-add-event! id "contract-success" nil)
    (events/send-event! (:seller-id contract) :contract/update contract)
    (events/send-event! (:buyer-id contract) :contract/update contract)))
