(ns oracle.actions
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
            [taoensso.sente.packers.transit :as sente-transit]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.database :as db]
            [oracle.common :as common]
            [oracle.tasks :as tasks]
            [oracle.events :as events]
            [oracle.escrow :as escrow]))

;;
;; Sente event handlers
;;

(defmulti -event-msg-handler "Multimethod to handle Sente `event-msg`s" :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  ;; (future (-event-msg-handler ev-msg)) ; Handle event-msgs on a thread pool
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (when ?reply-fn (?reply-fn {:umatched-event-as-echoed-from-from-server event})))

(defmethod -event-msg-handler :user/enter
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (try (let [{:keys [user friends] :as result}
             (db/user-insert! (:hashed-user ?data) (:hashed-friends ?data))]
         (?reply-fn {:status :ok
                     :found-user user
                     :found-friends friends}))
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :user/friends-of-friends
  [{:keys [?data ?reply-fn]}]
  (try (?reply-fn {:status :ok
                   :friends2 (db/get-user-friends-of-friends (:user-id ?data))})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :user/buy-requests
  [{:keys [?data ?reply-fn]}]
  (try (?reply-fn {:status :ok
                   :buy-requests (db/get-buy-requests-by-user (:user-id ?data))})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :user/contracts
  [{:keys [?data ?reply-fn]}]
  (try (?reply-fn {:status :ok
                   :contracts (db/get-contracts-by-user-with-last-event (:user-id ?data))})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/open
  [{:keys [?data ?reply-fn]}]
  (try (db/sell-offer-set! (:user-id ?data) (:currency ?data) (:min ?data) (:max ?data))
       (?reply-fn {:status :ok :min (:min ?data) :max (:max ?data)})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/get
  [{:keys [?data ?reply-fn]}]
  (try (let [{:keys [min max]} (db/sell-offer-get-by-user (:user-id ?data))]
         (cond
           (and min max)
           (?reply-fn {:status :ok :min min :max max})
           (and (not min) (not max))
           (?reply-fn {:status :ok})
           :else
           (?reply-fn (?reply-fn {:status :error :id :internal-mismatch-in-offer}))))
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/close
  [{:keys [?data ?reply-fn]}]
  (try (db/sell-offer-unset! (:user-id ?data))
       (?reply-fn {:status :ok})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/get-matches
  [{:keys [?data ?reply-fn]}]
  (try (?reply-fn {:offer-matches (db/get-buy-requests-by-counterparty (:user-id ?data))})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :buy-request/create
  [{:keys [?data ?reply-fn]}]
  (try (tasks/initiate-buy-request (:user-id ?data) (common/currency-as-long (:amount ?data) (:currency-seller ?data))
                                   (:currency-buyer ?data) (:currency-seller ?data))
       (?reply-fn {:status :ok})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defn preemptive-task-handler
  [{:keys [?data ?reply-fn]} tag]
  (try (tasks/initiate-preemptive-task tag ?data)
       (?reply-fn {:status :ok})
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :buy-request/accept
  [args]
  (preemptive-task-handler args :buy-request/accept))

(defmethod -event-msg-handler :buy-request/decline
  [args]
  (preemptive-task-handler args :buy-request/accept))

(defmethod -event-msg-handler :contract/break
  [args]
  (preemptive-task-handler args :contract/break))

(defmethod -event-msg-handler :contract/mark-transfer-sent
  [args]
  (preemptive-task-handler args :contract/mark-transfer-sent))

(defmethod -event-msg-handler :contract/mark-transfer-received
  [args]
  (preemptive-task-handler args :contract/mark-transfer-received))

(defmethod -event-msg-handler :escrow/get-user-key
  [{:keys [?data ?reply-fn]}]
  (if-let [key (case (:role ?data)
                 "buyer" (escrow/get-buyer-key (:id ?data))
                 "seller" (escrow/get-seller-key (:id ?data))
                 (?reply-fn {:error :unknown-role}))]
    (?reply-fn {:status :ok :escrow-user-key key})
    (?reply-fn {:error :unavailable-key})))

(defmethod -event-msg-handler :escrow/forget-user-key
  [{:keys [?data ?reply-fn]}]
  (case (:role ?data)
    "buyer"
    (try (db/contract-set-buyer-has-key! (:id ?data))
         (if (escrow/forget-buyer-key (:id ?data))
           (?reply-fn {:status :ok})
           (?reply-fn {:error :unavailable-key}))
         (catch Exception e (pprint e) (?reply-fn {:status :error})))
    "seller"
    (try (db/contract-set-seller-has-key! (:id ?data))
         (if (escrow/forget-seller-key (:id ?data))
           (?reply-fn {:status :ok})
           (?reply-fn {:error :unavailable-key}))
         (catch Exception e (pprint e) (?reply-fn {:status :error})))
    (?reply-fn {:error :unknown-role})))

(defmethod -event-msg-handler :escrow/release-to-user
  [{:keys [?data ?reply-fn]}]
  (if (or (empty? (:output-address ?data))
          (empty? (:escrow-user-key ?data)))
    (?reply-fn {:status :missing-parameters})
    ;; TODO: Validate key
    (try ;; TODO: Create a task to release the funds (:escrow-buyer-key ?data) in Redis
      (db/contract-set-output-address! (:id ?data) (:output-address ?data))
      (?reply-fn {:status :ok})
      (catch Exception e (pprint e) (?reply-fn {:status :error})))))

(defmethod -event-msg-handler :notification/ack
  [{:keys [?data ?reply-fn]}]
  (events/ack-notification (:user-hash ?data) (:uuid ?data))
  (?reply-fn {:status :ok}))

(defmethod -event-msg-handler :notification/get-pending
  [{:keys [?data ?reply-fn]}]
  (?reply-fn {:notifications (events/get-notifications (:user-hash ?data))}))

;;
;; Sente event router (`event-msg-handler` loop)
;;

(defonce router_ (atom nil))

(defn stop-sente-router! [] (when-let [stop-fn @router_] (stop-fn)))

(defn start-sente-router! [ch-chsk]
  (stop-sente-router!)
  (reset! router_ (sente/start-server-chsk-router! ch-chsk event-msg-handler)))
