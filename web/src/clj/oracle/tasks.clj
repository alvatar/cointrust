(ns oracle.tasks
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [taoensso.carmine.message-queue :as mq]
            [taoensso.nippy :as nippy]
            [cheshire.core :as json]
            [clj-time.core :as time]
            [clj-time.coerce :as time-coerce]
            ;; -----
            [oracle.database :as db]
            [oracle.events :as events]))

;;
;; Redis
;;

(def redis-conn {})
(defmacro wcar* [& body] `(r/wcar redis-conn ~@body))

;;
;; Utils
;;

(defn unix-now [] (time-coerce/to-long (time/now)))

(defn unix-after [reference duration] (time-coerce/to-long (time/plus reference duration)))

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

(defn initiate-buy-request [buyer-id amount currency-buy currency-sell]
  (log/debug (format "Initiated buy request from buyer ID %d for %d %s" buyer-id amount currency-buy))
  (wcar* (mq/enqueue (get-in workers [:buy-requests-master :qname])
                     {:buyer-id buyer-id :amount amount
                      :currency-buy currency-buy :currency-sell currency-sell})))

(defn initiate-contract [buy-request]
  (if (:id buy-request)
    (log/debug "Initiated contracts from buy request ID" (:id buy-request))
    (throw (Exception. "The buy request doesn't have an seller ID")))
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
(defn pick-counterparty [buyer-id amount currency-sell]
  (let [[available _ _] (diff (db/get-user-friends-of-friends buyer-id)
                              (wcar* (r/smembers (str "buyer->blacklist:" buyer-id))))]
    (rand-nth available)))

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
        now (unix-now)
        [_ stored _] (wcar* (r/hsetnx op-key :global (json/generate-string {:created now :started now}))
                            (r/hgetall op-key)
                            (r/expire op-key ops-ttl))]
    (assoc-in (into {} (for [[k v] (partition 2 stored)] [(keyword k) (json/parse-string v true)]))
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
  (let [{:keys [buyer-id amount currency-buy currency-sell]} message
        state (idempotency-state-rebuild mid)
        now (get-in state [:global :now])
        buy-request (with-idempotent-transaction mid :buy-request-create state
                      #(if-let [result (db/buy-request-create! buyer-id amount currency-buy currency-sell 1000.0 %)]
                         (do (log/debug "Buy request created:" result) result)
                         (throw (Exception. "Couldn't create buy-request"))))
        buy-request-id (:id buy-request)]
    ;; TODO: retrieve exchange rate (probably a background worker updating a Redis key)
    (log/debugf "STATE: %s" state)
    (log/debug "Processing buy request ID" buy-request-id)
    (idempotent-ops mid :event-buy-request-created state
                    (events/dispatch! buyer-id :buy-request-created buy-request))
    (Thread/sleep 3000)
    (if-let [seller-id (with-idempotent-transaction mid :pick-counterparty state
                         #(db/buy-request-set-seller! buy-request-id (pick-counterparty buyer-id amount currency-sell) %))]
      (do (idempotent-ops mid :event-sell-offer-matched state
                          (events/dispatch! seller-id :sell-offer-matched buy-request)
                          (events/dispatch! buyer-id :buy-request-matched {:id buy-request-id :seller-id seller-id}))
          ;; Here we check if its accepted. If so, the task succeeds. Handle timeout waiting for response.
          ;; (log/debugf "%s seconds have passed since this task was created." (float (/ (- now (:started (:global state))) 1000)))
          (let [buy-request-status (get-buy-request-status buy-request-id)]
            (cond
              ;; Buy request accepted
              (= buy-request-status "<accepted>")
              (do (clear-user-blacklist buyer-id)
                  {:status :success})
              ;; Buy request declined
              (= buy-request-status "<declined>")
              (do (idempotency-state-set! mid :global (merge (:global state) {:started now}))
                  (idempotency-state-set! mid :pick-counterparty nil)
                  (idempotency-state-set! mid :event-sell-offer-matched nil)
                  (blacklist-counterparty buyer-id seller-id)
                  (db/buy-request-unset-seller! buy-request-id) ; Idempotent
                  (clear-buy-request-status buy-request-id)
                  {:status :retry :backoff-ms 1})
              ;; Time has passed. Search another seller immediately.
              (> now (unix-after (time-coerce/to-date-time (:created buy-request)) (time/days 1)))
              (do (log/debugf "Buy request ID %d has timedout since no action has been taken by the seller" buy-request-id)
                  (db/buy-request-unset-seller! buy-request-id)
                  (blacklist-counterparty buyer-id seller-id)
                  (events/dispatch! seller-id :buy-request-restart buy-request)
                  {:status :retry :backoff-ms 1})
              :else
              {:status :retry :backoff-ms 4000})))
      (do (log/debug "No seller match. Retrying in 5 minutes.")
          {:status :retry :backoff-ms 300000}))))

;;
;;
;;TODO: HANDLE EXCEPTIONS PROPERLY IN TASKS
;; RECOVER THEM, ETC
;;
;;



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
  (let [{:keys [id] :as buy-request} message
        state (idempotency-state-rebuild mid)
        now (get-in state [:global :now])
        initial-contract (with-idempotent-transaction mid :contract state
                           #(if-let [result (db/contract-create! buy-request %)]
                              (do (log/debug "Contract created:" result) result)
                              (throw (Exception. "Couldn't create contract"))))
        contract-id (:id initial-contract)
        ;; Make sure we have the latest version of the contrast (forget idempotent op)
        contract (db/get-contract-by-id-with-last-event contract-id)]
    (log/debugf "Attempt %d for contract ID %s. Contract: %s" attempt contract-id contract)
    ;; Task initialization
    (when (= attempt 1)
      (events/dispatch! (:buyer-id contract) :contract-create contract)
      (events/dispatch! (:seller-id contract) :contract-create contract))
    (case (:stage contract)
      ;; Currently only used for testing
      "contract-success"
      {:status :success}
      ;; Contract can be cancelled anytime
      "contract-broken"
      (do (events/dispatch! (:buyer-id contract) :contract-broken contract)
          (events/dispatch! (:seller-id contract) :contract-broken contract)
          (db/contract-set-escrow-open-for! contract (:seller-id contract))
          {:status :success})
      ;; We inform of the expected transaction and the freezing of price.
      ;; We wait for the seller to fund the escrow and provide the transfer details.
      "waiting-escrow"
      (cond (and (:transfer-info contract)
                 (:escrow-funded contract)) ; Observing the blockchain
            (do (with-idempotent-transaction mid :contract-add-event-waiting-transfer state
                  (fn [idemp]
                    (log/debug "Contract stage changed to \"waiting-transfer\"")
                    (db/contract-add-event! contract-id "waiting-transfer" nil idemp)))
                (events/dispatch! (:buyer-id contract) :contract-waiting-transfer contract)
                {:status :retry :backoff-ms 1})
            (> now (unix-after (time-coerce/to-date-time (:created contract)) (time/days 1)))
            (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                  #(db/contract-add-event! contract-id "contract-broken" {:reason "escrow waiting period timed out"} %))
                {:status :retry :backoff-ms 1})
            :else
            {:status :retry :backoff-ms 1000})
      ;; We provide the buyer with A) the transfer info B) the instructions
      ;; Buyer informs of transfer performed to the system
      ;; The seller is informed of the transfer initiated
      "waiting-transfer"
      (cond (:transfer-received contract)
            (do (with-idempotent-transaction mid :contract-add-event-holding-period state
                  (fn [idemp]
                    (log/debug "Contract stage changed to \"holding-period\"")
                    (db/contract-add-event! contract-id "holding-period" nil idemp)))
                (events/dispatch! (:buyer-id contract) :contract-holding-period contract)
                (events/dispatch! (:seller-id contract) :contract-holding-period contract)
                {:status :retry :backoff-ms 1})
            ;; The transfer waiting period includes both buyer sending and seller receiving notifications
            (> now (unix-after (time-coerce/to-date-time (:waiting-transfer-start contract)) (time/days 1)))
            (do (with-idempotent-transaction mid :contract-add-event-contract-boken state
                  #(db/contract-add-event! contract-id "contract-broken" {:reason "transfer waiting period timed out"} %))
                {:status :retry :backoff-ms 1})
            :else
            {:status :retry :backoff-ms 6000})
      ;; Seller informs of transfer received
      ;; The buyer is informed of the transfer received
      "holding-period"
      (cond (> now (unix-after (time-coerce/to-date-time (:holding-period-start contract)) (time/days 100)))
            (do (db/contract-set-escrow-open-for! contract (:buyer-id contract)) ; Idempotent
                (with-idempotent-transaction mid :contract-success state
                  #(db/contract-add-event! contract-id "contract-success" nil %))
                {:status :success})
            :else
            {:status :retry :backoffs-ms 360000}))))

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
    (events/dispatch! (:buyer-id buy-request) :buy-request-accept buy-request) ; Repeat OK
    (initiate-contract buy-request) ; Idempotent
    ;; Keep in mind that we are deleting the request here, so we rely on the master task
    ;; to retrieve the buy request info from the idempotency cache in Redis
    (db/buy-request-delete! buy-request-id) ; Idempotent, must be done at the end
    {:status :success}))

(defmethod common-preemptive-handler :buy-request/decline
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<declined>") ; Idempotent
    (events/dispatch! (:buyer-id buy-request) :buy-request-decline buy-request)
    {:status :success}))

(defmethod common-preemptive-handler :contract/break
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    ;; TODO: make sure this one is the last one somehow, or at least seen!
    (with-idempotent-transaction mid :preemptive-contract-broken
      #(db/contract-add-event! contract-id "contract-broken" nil %))
    {:status :success}))

(defmethod common-preemptive-handler :contract/mark-transfer-sent
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    (db/contract-set-transfer-sent! contract-id)
    (events/dispatch! (:seller-id contract) :contract-mark-transfer-sent-ack contract) ; Allow repetition
    (events/dispatch! (:buyer-id contract) :contract-mark-transfer-sent-ack contract) ; Allow repetition
    {:status :success}))

(defmethod common-preemptive-handler :contract/mark-transfer-received
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        contract-id (:id data)
        contract (db/get-contract-by-id contract-id)]
    (log/debug message)
    (db/contract-set-transfer-received! contract-id)
    (events/dispatch! (:buyer-id contract) :contract-mark-transfer-received-ack contract) ; Allow repetition
    (events/dispatch! (:seller-id contract) :contract-mark-transfer-received-ack contract) ; Allow repetition
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

(defonce start-workers_ (workers-start!))

(defn flush-redis!!! [] (wcar* (r/flushall)))

(defn total-wipeout!!! []
  (workers-stop!)
  (flush-redis!!!)
  (db/reset-database!!!)
  (db/populate-test-database!!!)
  (workers-start!))


;; Decline buy request
;; (oracle.tasks/initiate-preemptive-task :buy-request/decline {:id 1})

;; Accept buy request
;; (oracle.database/buy-request-set-seller! 1 3)
;; (oracle.tasks/initiate-preemptive-task :buy-request/accept {:id 1})

;; Directly create contract
;; (oracle.tasks/initiate-contract {:id 1, :created #inst "2017-01-16T18:22:07.389569000-00:00", :buyer-id 1, :seller-id 4, :amount 100000000, :currency-buy "usd", :currency-sell "xbt", :exchange-rate 1000.000000M})

;; Seller sends money to escrow
;; (oracle.database/contract-set-escrow-funded! 1 "put it here")

;; Seller marks transfer received
;; (oracle.tasks/initiate-preemptive-task :contract/mark-transfer-received {:id 1})

;; Force contract success
;; (db/contract-add-event! 1 "contract-success" nil)
