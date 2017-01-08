(ns oracle.tasks
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [taoensso.carmine.message-queue :as mq]
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
;; Matching
;;

;; This is the core of Cointrust. When a request to buy is received,
;; the matching engine will select a counterparty (seller), wait for
;; confirmation, and then create a contract and notify both parties.
(defn pick-counterparty [user-id amount currency-sell]
  (rand-nth (db/get-user-friends-of-friends user-id)))

;;
;; Tasks
;;

(defn initiate-buy-request [buyer-id amount currency-buy currency-sell]
  (wcar* (mq/enqueue "buy-requests-queue"
                     {:buyer-id buyer-id :amount amount
                      :currency-buy currency-buy :currency-sell currency-sell})))

(defn initiate-contract [buyer-id seller-id amount currency-buy currency-sell exchange-rate]
  (wcar* (mq/enqueue "contracts-queue"
                     {:buyer-id buyer-id :seller-id seller-id})))

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

(defn unix-now [] (time-coerce/to-long (time/now)))

(defn unix-after [duration] (time-coerce/to-long (time/plus (time/now) duration)))

(defn reveal-idempotency-state [uid]
  (let [ops-ttl (* 7 24 3600)
        op-key (str "idempotent-ops:" uid)
        [_ stored _] (wcar* (r/hsetnx op-key "global" {:created (unix-now)})
                            (r/hgetall op-key)
                            (r/expire op-key ops-ttl))]
    (into {} (for [[k v] (partition 2 stored)] [(keyword k) v]))))

(defn idempotent-op* [uid tag state operation]
  (let [op-key (str "idempotent-ops:" uid)
        field (name tag)]
    (if-let [stored (tag state)]
      (json/parse-string stored)
      (let [op-result (operation)
            store-result (if (map? op-result) op-result "<output-not-map>")]
        (wcar* (r/hset op-key field (json/generate-string store-result)))
        op-result))))

(defmacro idempotent-op [uid tag state & body]
  `(idempotent-op* ~uid ~tag ~state (fn [] ~@body)))

(defn buy-requests-handler
  [{:keys [mid message attempt] :as all}]
  (let [{:keys [buyer-id amount currency-buy currency-sell]} message
        state (reveal-idempotency-state mid)]
    (log/debug "STATE:" state)
    ;; TODO: retrieve exchange rate (probably a background worker updating a Redis key)
    (let [buy-request
          (idempotent-op mid :buy-request-create state
                         (if-let [result (db/buy-request-create! buyer-id amount currency-buy currency-sell 1000.0)]
                           (do (log/debug "Buy request created:" result) result)
                           (throw (Exception. "Couldn't create buy-request"))))]
      (idempotent-op mid :event-buy-request-created state
                     (events/dispatch! buyer-id :buy-request-created buy-request))
      (Thread/sleep 5000) ;; FAKE
      (if-let [seller-id (idempotent-op mid :pick-counterparty state
                                        (db/buy-request-set-seller! (:id buy-request) (pick-counterparty buyer-id amount currency-sell)))]
        (do (idempotent-op mid :sell-offer-matched state
                           (events/dispatch! seller-id :sell-offer-matched buy-request)
                           (events/dispatch! buyer-id :buy-request-matched {:id (:id buy-request) :seller-id seller-id}))
            ;; Here we check if its accepted. If so, the task succeeds. Handle timeout waiting for response.
            (println (format "%s seconds have passed since this task was created." (\ (- (unix-now) (:created (:global state))) 1000)))
            (let [buy-request-status (get-buy-request-status (:id buy-request))]
              (cond
                (= buy-request-status "accepted") ; Buy request accepted
                (do
                  ;; --- Do as preemptive event. Micro task.
                  (initiate-contract buyer-id seller-id ...) ; XXX
                  (db/buy-request-delete! (:id buy-request)) ; XXX
                  (events/dispatch! seller-id :buy-request-accepted buy-request)  ; XXX
                  ;; ---
                  {:status :success}
                  )
                (= buy-request-status "declined") ; Buy request declined
                (do
                  (db/buy-request-unset-seller! (:id buy-request))
                  (blacklist-counterparty buyer-id seller-id) ; XXX
                  (events/dispatch! seller-id :buy-request-declined buy-request) ; XXX
                  {:status :retry :backoff-ms 1})
                (< (unix-now) (unix-after (time/days 1))) ; Still within time window. Check again in 20 seconds
                {:status :retry :backoff-ms 20000}
                :else ; Time has passed. Search another seller immediately.
                (do
                  (db/buy-request-unset-seller! (:id buy-request))
                  (blacklist-counterparty buyer-id seller-id) ; XXX
                  (events/dispatch! seller-id :buy-request-restarted buy-request)
                  {:status :retry :backoff-ms 1}))))
        ;; Retry after 1 minute
        (do (log/debug "No seller match. Retrying in 5 minutes.")
            ;; (log/debug (db/get-user-friends-of-friends buyer-id))
            {:status :retry :backoff-ms 300000})))))

;; TODO:
;; Preemptive events: micro tasks (one single micro-task handler). They mark
;;   the accomplished micro-task, for the master task to drive the progress.
;; Checks: master tasks

(defn contracts-handler
  [{:keys [mid message attempt]}]
  (let [{:keys [id buyer-id seller-id amount currency]} message
        state (reveal-idempotency-state mid)
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
;; Lifecyle
;;

(defonce workers {:buy-requests {:qname "buy-requests-queue"
                                 :handler (ref nil)
                                 :worker (ref nil)}
                  :contracts {:qname "contracts-queue"
                              :handler (ref nil)
                              :worker (ref nil)}})

(def worker-handlers {:buy-requests buy-requests-handler
                      :contracts contracts-handler})

(defn buy-requests-queue-state [] (mq/queue-status redis-conn (get-in [:buy-requests :qname] workers)))
(defn contracts-queue-state [] (mq/queue-status redis-conn (get-in [:contracts :qname] workers)))

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
