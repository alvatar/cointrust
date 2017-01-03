(ns oracle.actions
  (:require [clojure.pprint :refer [pprint]]
            ;; Environment and configuration
            [environ.core :refer [env]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
            [taoensso.sente.packers.transit :as sente-transit]
            ;; Internal
            [oracle.database :as db]
            [oracle.tasks :as tasks]))

;;
;; Sente event handlers
;;

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  ;; Dispatch on event-id
  :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg) ; Handle event-msgs on a single thread
  ;; (future (-event-msg-handler ev-msg)) ; Handle event-msgs on a thread pool
  )

(defmethod -event-msg-handler :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid (:uid session)]
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

(defmethod -event-msg-handler :user/enter
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (try (let [{:keys [user friends] :as result}
             (db/user-insert! (:hashed-user ?data) (:hashed-friends ?data))]
         (?reply-fn {:status :ok
                     :found-user user
                     :found-friends friends}))
       (catch Exception e
         (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :user/friends-of-friends
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (try (?reply-fn {:status :ok
                   :friends2 (db/get-user-friends-of-friends (:user-id ?data))})
       (catch Exception e
         (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :user/contracts
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (try (?reply-fn {:status :ok
                   :contracts (for [c (db/get-user-contracts (:user-id ?data))]
                                (merge c (db/get-contract-last-event (:id c))))})
       (catch Exception e
         (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/open
  [{:as ev-msg :keys [event uid id ?data ring-req ?reply-fn send-fn]}]
  (try (let [res (db/offer-set! (:user-id ?data) (:min ?data) (:max ?data))]
         (if (= res 'ok)
           (?reply-fn {:status :ok
                       :min (:min ?data)
                       :max (:max ?data)})
           (pprint res)))
       (catch Exception e
         (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/get
  [{:as ev-msg :keys [event uid id ?data ring-req ?reply-fn send-fn]}]
  (try (let [{:as asff :keys [min max]}
             (db/offer-get-by-user (:user-id ?data))]
         (when (and min max)
           (?reply-fn {:status :ok :min min :max max})))
       (catch Exception e
         (?reply-fn {:status :error}))))

(defmethod -event-msg-handler :offer/close
  [{:as ev-msg :keys [event uid id ?data ring-req ?reply-fn send-fn]}]
  (try (let [res (db/offer-unset! (:user-id ?data))]
         (if (= res 'ok)
           (?reply-fn {:status :ok})
           (pprint res)))
       (catch Exception e
         (?reply-fn {:status :error}))))

(declare task-init-contract)

(defmethod -event-msg-handler :contract/request
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (try (tasks/init-contract (:user-id ?data) (:btc ?data))
       (?reply-fn {:status :ok})
       (catch Exception e
         (?reply-fn {:status :error}))))

;;
;; Sente event router (`event-msg-handler` loop)
;;

(defonce router_ (atom nil))

(defn stop-sente-router! [] (when-let [stop-fn @router_] (stop-fn)))

(defn start-sente-router! [ch-chsk]
  (stop-sente-router!)
  (reset! router_
          (sente/start-server-chsk-router!
           ch-chsk
           event-msg-handler)))
