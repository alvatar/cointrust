(ns oracle.tasks
  (:require [clojure.pprint :refer [pprint]]
            ;; Environment and configuration
            [environ.core :refer [env]]
            ;; Redis tasks
            [taoensso.carmine :as r]
            [taoensso.carmine.message-queue :as mq]
            ;; Internal
            [oracle.database :as db]))

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
(defn pick-counterparty [user-id btc]
  (rand-nth (db/get-user-friends-of-friends user-id)))

(defn get-counterparty [buyer-id]
  (wcar* (r/hget "contract:buyer->seller" buyer-id)))

(defn store-counterparty [buyer-id seller-id]
  (wcar* "contract:buyer->seller" buyer-id seller-id))

;;
;; Tasks
;;

(defn request-contract [buyer-id amount & [currency]]
  (wcar* (mq/enqueue "requested-contracts-queue"
                     {:buyer-id buyer-id :amount amount :currency currency}))
  (db/buy-request-create! buyer-id amount (or currency "xbt")))

(defn activate-contract [buyer-id seller-id btc]
  (wcar* (mq/enqueue "active-contracts-queue"
                     {:buyer-id buyer-id :seller-id seller-id :btc btc})))

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

(defn request-contract-handler
  [{:keys [message attempt]}]
  (let [{:keys [buyer-id btc stage]} message
        seller-id (or (get-counterparty buyer-id)
                      (store-counterparty buyer-id (pick-counterparty buyer-id btc)))]
    (println "Contract REQUEST")
    {:status :success}))

(defn active-contract-handler
  [{:keys [message attempt]}]
  (let [{:keys [buyer-id btc stage]} message
        seller-id (or (get-counterparty buyer-id)
                      (store-counterparty buyer-id (pick-counterparty buyer-id btc)))]
    (println "Contract ACTIVE")
    {:status :success}))

;;
;; Lifecyle
;;

(defonce contract-workers
  {:request {:qname "requested-contracts-queue"
             :handler request-contract-handler
             :worker (ref nil)}
   :active {:qname "active-contracts-queue"
            :handler active-contract-handler
            :worker (ref nil)}})

(defn contract-workers-stop! []
  (dosync
   (doseq [[stage {:keys [worker]}] contract-workers]
     (when @worker (mq/stop @worker)))))

(defn contract-workers-start! []
  (dosync
   (doseq [[stage worker] contract-workers]
     (alter (:worker worker)
            #(or % (mq/worker redis-conn
                              (:qname worker)
                              {:eoq-backoff-ms 200
                               :throttle-ms 200
                               :handler (:handler worker)}))))))

(defonce start-contract-workers_ (contract-workers-start!))
