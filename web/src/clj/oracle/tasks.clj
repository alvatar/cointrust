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

(defn init-contract [buyer-id btc]
  (wcar* (mq/enqueue "contract-requests-queue"
                     {:buyer-id buyer-id :btc btc :stage :request})))

;; 1. Pick counterparty (or multiple), and wait for response
;; counterparty (pick-counterparty buyer-id)
;; 2. If no response, pick another one
;; 3. When counterparty accepts, trigger contract creation
;; (db/contract-create! buyer-id counterparty (:btc ?data))
;; Notifications are done here

(def contract-worker-handlers
  {:request (fn [{:keys [message attempt]}]
              (let [{:keys [buyer-id btc stage]} message
                    seller-id (or (get-counterparty buyer-id)
                                  (store-counterparty buyer-id (pick-counterparty buyer-id btc)))]
                (case stage
                  :request
                  (println "Contract in REQUEST stage"))
                {:status :success}))})

;;
;; Lifecyle
;;

(defonce contract-workers
  {:request (ref nil)})

(defn contract-workers-stop! []
  (dosync
   (doseq [[stage handler] contract-workers]
     (when @handler (mq/stop @handler)))))

(defn contract-workers-start! []
  (contract-workers-stop!)
  (dosync
   (doseq [[stage handler] contract-workers]
     (ref-set (stage contract-workers)
              (mq/worker redis-conn
                         "contract-requests-queue"
                         {:eoq-backoff-ms 200
                          :throttle-ms 200
                          :handler (stage contract-worker-handlers)})))))

(defonce start-contract-workers_ (contract-workers-start!))
