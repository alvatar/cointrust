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
            [oracle.escrow :as escrow]
            [oracle.currency :as currency]))

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

(defmethod -event-msg-handler :currency/get-exchange-rates
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (?reply-fn {:exchange-rates (currency/get-current-exchange-rates)}))

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
  (try (let [{:keys [user-id min max currency]} ?data
             premium 100]
         (db/sell-offer-set! user-id currency
                             (common/currency-as-long min currency)
                             (common/currency-as-long max currency)
                             premium)
         (?reply-fn {:min min :max max :currency currency :premium premium}))
       (catch Exception e (pprint e) (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/get
  [{:keys [?data ?reply-fn]}]
  (try (let [{:keys [min max currency premium]} (db/get-sell-offer-by-user (:user-id ?data))]
         (cond
           (and min max)
           (?reply-fn {:min min :max max :currency currency :premium premium})
           (and (not min) (not max))
           (?reply-fn {:status :no-offer})
           :else
           (?reply-fn {:status :error :message "Internal mismatch in offer"})))
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
  (preemptive-task-handler args :buy-request/decline))

(defmethod -event-msg-handler :contract/break
  [args]
  (preemptive-task-handler args :contract/break))

(defmethod -event-msg-handler :contract/mark-transfer-received
  [args]
  (preemptive-task-handler args :contract/mark-transfer-received))

(defmethod -event-msg-handler :escrow/get-user-key
  [{:keys [?data ?reply-fn]}]
  (if-let [key (case (:role ?data)
                 "buyer" (escrow/encode-key (escrow/get-buyer-key (:id ?data)))
                 "seller" (escrow/encode-key (escrow/get-seller-key (:id ?data)))
                 (?reply-fn {:error :unknown-role}))]
    (?reply-fn {:status :ok :escrow-user-key key})
    (?reply-fn {:error :unavailable-key})))

(defmethod -event-msg-handler :escrow/forget-user-key
  [{:keys [?data ?reply-fn]}]
  (case (:role ?data)
    "buyer"
    (try (db/contract-set-field! (:id ?data) "escrow_buyer_has_key" true)
         (if (escrow/forget-buyer-key (:id ?data))
           (?reply-fn {:status :ok})
           (?reply-fn {:error :unavailable-key}))
         (catch Exception e (pprint e) (?reply-fn {:status :error})))
    "seller"
    (try (db/contract-set-field! (:id ?data) "escrow_seller_has_key" true)
         (if (escrow/forget-seller-key (:id ?data))
           (?reply-fn {:status :ok})
           (?reply-fn {:error :unavailable-key}))
         (catch Exception e (pprint e) (?reply-fn {:status :error})))
    (?reply-fn {:error :unknown-role})))

(defmethod -event-msg-handler :escrow/release-to-user
  [{:keys [?data ?reply-fn]}]
  (let [output-address (clojure.string/trim (:output-address ?data))
        escrow-user-key (clojure.string/trim (:escrow-user-key ?data))]
   (if (or (empty? output-address)
           (empty? escrow-user-key))
     (?reply-fn {:status :error-missing-parameters})
     ;; TODO check validity of address before initiating task
     (try (db/contract-set-field! (:id ?data) "output_address" output-address)
          (db/contract-set-field! (:id ?data) "escrow_release" "<processing>")
          (tasks/initiate-preemptive-task :escrow/release-to-user ?data)
          (?reply-fn {:status :ok})
          (catch Exception e (pprint e) (?reply-fn {:status :error}))))))

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

(defn sente-router-stop! [] (when-let [stop-fn @router_] (stop-fn)))

(defn sente-router-start! [ch-chsk]
  (sente-router-stop!)
  (reset! router_ (sente/start-server-chsk-router! ch-chsk event-msg-handler)))
