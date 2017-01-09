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

(defn unix-after [duration] (time-coerce/to-long (time/plus (time/now) duration)))

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

(defn initiate-contract [buy-request-id]
  (log/debug "Initiated contracts from buy request ID" buy-request-id)
  (throw (Exception. "NOT IMPLEMENTED"))
  (wcar* (mq/enqueue (get-in workers [:contracts-master-queue :qname])
                     {:TODO :TODO})))

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

;; 1. Pick counterparty (or multiple), and wait for response
;; counterparty (pick-counterparty buyer-id)
;; 2. If no response, pick another one
;; 3. When counterparty accepts, trigger contract creation
;; (db/contract-create! buyer-id counterparty (:btc ?data))
;; Notifications are done here

;; - REQUEST
;; 1. Recover counterparty, or run matching algorithm if buyer not matched.
;;    If no counterparty, abort.
;; 2. Do notifications: seller via web push, email with a click-to-accept link, and
;;    set "pending" status somehow so if the seller logs in, gets notified.
;; 3. Accept/decline in any of the notifications (and immediately request data from next stage)
;; 4. If declined/timed-out, mark counterparty as blacklisted, and retry immediately.
;; 5. If accepted, create contract
;; - RUNNING CONTRACT
;; 1. Get current stage
;; 2. Check if stage "notified". If not, do it.
;; 3. Check for conditions to change stage. If not met, retry later.
;; 4. Update SQL db.
;; 5. Set new stage
;; 6. Notify/Set-aync-notification combo
;; 7. Set as stage "notified"

;;
;; Workers
;;

(defn idempotency-state-reveal [uid]
  (let [ops-ttl (* 7 24 3600) ; 1 week
        op-key (str "idempotent-ops:" uid)
        [_ stored _] (wcar* (r/hsetnx op-key :global (let [now (unix-now)] {:created now :started now}))
                            (r/hgetall op-key)
                            (r/expire op-key ops-ttl))]
    (into {} (for [[k v] (partition 2 stored)] [(keyword k) v]))))

(defn idempotency-state-merge! [uid new-map]
  (let [op-key (str "idempotent-ops:" uid)
        global (wcar* (r/hget op-key :global))]
    (wcar* (r/hset op-key :global (merge global new-map)))))

(defn idempotent-op* [uid tag state operation]
  (let [op-key (str "idempotent-ops:" uid)
        field (name tag)]
    (if-let [stored (tag state)]
      (json/parse-string stored true)
      (let [op-result (operation)]
        (wcar* (r/hset op-key field (try (json/generate-string op-result)
                                         ;; "null" is serialized as a null value, but found in Redis
                                         (catch Exception e "null"))))
        op-result))))

(defmacro idempotent-op [uid tag state & body]
  `(idempotent-op* ~uid ~tag ~state (fn [] ~@body)))

;;
;; Master task queues: they track progress of an entity. They have time and state.
;;

(defn buy-requests-master-handler
  [{:keys [mid message attempt] :as all}]
  (let [{:keys [buyer-id amount currency-buy currency-sell]} message
        state (idempotency-state-reveal mid)]
    (log/debug "STATE:" state)
    ;; TODO: retrieve exchange rate (probably a background worker updating a Redis key)
    (let [buy-request
          (idempotent-op mid :buy-request-create state
                         (if-let [result (db/buy-request-create! buyer-id amount currency-buy currency-sell 1000.0)]
                           (do (log/debug "Buy request created:" result) result)
                           (throw (Exception. "Couldn't create buy-request"))))
          buy-request-id (:id buy-request)]
      (log/debug "Processing buy request ID" buy-request-id)
      (idempotent-op mid :event-buy-request-created state
                     (events/dispatch! buyer-id :buy-request-created buy-request))
      (Thread/sleep 5000) ;; FAKE
      (if-let [seller-id (idempotent-op mid :pick-counterparty state
                                        (db/buy-request-set-seller! buy-request-id (pick-counterparty buyer-id amount currency-sell)))]
        (do (log/debug (format "Request ID %d is matched with seller ID %d" buy-request-id seller-id))
            (idempotent-op mid :sell-offer-matched state
                           (events/dispatch! seller-id :sell-offer-matched buy-request)
                           (events/dispatch! buyer-id :buy-request-matched {:id buy-request-id :seller-id seller-id}))
            ;; Here we check if its accepted. If so, the task succeeds. Handle timeout waiting for response.
            (log/debug (format "%s seconds have passed since this task was created." (float (/ (- (unix-now) (:started (:global state))) 1000))))
            (let [buy-request-status (get-buy-request-status buy-request-id)]
              (cond
                ;; Buy request accepted
                (= buy-request-status "<accepted>")
                (do (db/buy-request-delete! buy-request-id)
                    (clear-user-blacklist buyer-id)
                    {:status :success})
                ;; Buy request declined
                (= buy-request-status "<declined>")
                (do (idempotency-state-merge! mid {:started (unix-now) :pick-counterparty nil})
                    (blacklist-counterparty buyer-id seller-id)
                    (db/buy-request-unset-seller! buy-request-id)
                    {:status :retry :backoff-ms 1})
                ;; Still within time window. Check again in 60 seconds.
                (< (unix-now) (unix-after (time/days 1)))
                {:status :retry :backoff-ms 60000}
                ;; Time has passed. Search another seller immediately.
                :else
                (do (db/buy-request-unset-seller! buy-request-id)
                    (blacklist-counterparty buyer-id seller-id)
                    (events/dispatch! seller-id :buy-request-restarted buy-request)
                    {:status :retry :backoff-ms 1}))))
        (do (log/debug "No seller match. Retrying in 5 minutes.")
            {:status :retry :backoff-ms 300000})))))

(defn contracts-master-handler
  [{:keys [mid message attempt]}]
  #_(let [{:keys [id buyer-id seller-id amount currency]} message
        state (idempotency-state-reveal mid)
        contract-stage (get-contract-state id)]
    (log/debug "STATE:" state)
    (case contract-stage
      ;; Price is frozen
      ;; We provide the buyer with A) the transfer info B) the instructions
      ;; We inform the seller of the expected transaction and the freezing of price
      "stage-A"
      (do)
      ;; Buyer informs of transfer performed to the system
      ;; The seller is informed of the transfer initiated
      "stage-B"
      (do)
      ;; Seller informs of transfer performed
      ;; The buyer is informed of the transfer received
      "stage-C"
      (do)
      ;; 100 days holding period (chargeback protection)
      "stage-D"
      (do)
      ;; Inform both the buyer and seller of the end of the holding period
      "stage-E"
      (do))
    ;; Retry in 20 seconds
    {:status :retry :backoff-ms 20000}))

;;
;; Preemptive task queues: They mark the accomplished micro-task, for the master
;; task to drive the progress.
;; Important: inside these tasks, only thing that *need to be performed ASAP* should
;; be placed. Everything else should go in the master tasks.
;;

(defmulti common-preemptive-handler
  (fn [{:keys [mid message attempt]}] (:tag message)))

(defmethod common-preemptive-handler :buy-request/accepted
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<accepted>") ; Idempotent
    (events/dispatch! (:seller-id buy-request) :buy-request-accepted buy-request) ; Repeat OK
    (initiate-contract buy-request)
    {:status :success}))

(defmethod common-preemptive-handler :buy-request/declined
  [{:keys [mid message attempt]}]
  (let [{:keys [tag data]} message
        buy-request-id (:id data)
        buy-request (db/get-buy-request-by-id buy-request-id)]
    (log/debug message)
    (set-buy-request-status buy-request-id "<declined>") ; Idempotent
    (events/dispatch! (:seller-id buy-request) :buy-request-declined buy-request)
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

(defn flushall!!! [] (wcar* (r/flushall)))
