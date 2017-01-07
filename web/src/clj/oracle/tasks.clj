(ns oracle.tasks
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [taoensso.carmine.message-queue :as mq]
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

(defn get-counterparty [buy-request-id]
  (wcar* (r/hget "buy-request->seller" buy-request-id)))

(defn store-counterparty [buy-request-id seller-id]
  (db/buy-request-set-seller! buy-request-id seller-id)
  (wcar* "buy-request->seller" buy-request-id seller-id)
  seller-id)

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
;; 4. If declined/timedout, mark counterparty as blacklisted, and retry immediately.
;; 5. If accepted, create contract
;; - RUNNING CONTRACT
;; 1. Get current stage
;; 2. Check if stage "notified". If not, do it.
;; 3. Check for conditions to change stage. If not met, retry later.
;; 4. Update SQL db.
;; 5. Set new stage
;; 6. Notify/Set-aync-notification combo
;; 7. Set as stage "notified"

(defn buy-requests-handler
  [{:keys [message attempt]}]
  (let [{:keys [buyer-id amount currency-buy currency-sell]} message]
    ;; TODO: retrieve exchange rate
    ;; TODO: IDEMPOTENT REQUEST CREATE
    (let [buy-request (db/buy-request-create! buyer-id amount currency-buy currency-sell 1000.0)]
      (when-not buy-request (throw (Exception. "Couldn't create buy-request")))
      (events/dispatch! buyer-id :buy-request-created buy-request)
      (log/debug "Buy request created:" buy-request)
      (Thread/sleep 5000) ;; FAKE
      (if-let [seller-id (or (get-counterparty buyer-id)
                             (store-counterparty (:id buy-request)
                                                 (pick-counterparty buyer-id amount currency-sell)))]
        (do (events/dispatch! buyer-id :buy-request-matched {:id (:id buy-request) :seller-id seller-id})
            {:status :success})
        (do (log/debug "No seller match")
            (log/debug (db/get-user-friends-of-friends buyer-id))
            {:status :success})))))

(defn contracts-handler
  [{:keys [message attempt]}]
  (let [{:keys [buyer-id seller-id amount currency]} message]
    (println "Contract ACTIVE")
    {:status :success}))

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
