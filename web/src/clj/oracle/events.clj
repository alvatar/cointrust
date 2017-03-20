(ns oracle.events
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [cheshire.core :as json]
            ;; -----
            [oracle.database :as db]
            [oracle.redis :refer :all]))


;;
;; Events
;;

(defn init! [chsk-send!_] (def chsk-send! chsk-send!_))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn store-notification [uid notif-uuid args]
  (wcar* (r/hset (str "user->notifications:" uid)
                 notif-uuid
                 (json/generate-string args))))

(defn ack-notification [uid notif-uuid]
  (wcar* (r/hdel (str "user->notifications:" uid)
                 notif-uuid)))

(defn notification [uid title message]
  (let [notif-uuid (uuid)]
    ;; (store-notification uid notif-uuid {:title title :message message})
    (chsk-send! uid [:notification/create {:uuid notif-uuid :title title :message message}])))

(defn get-notifications [uid]
  (for [[uuid notif] (redis->json (wcar* (r/hgetall (str "user->notifications:" uid))))]
    (merge notif {:uuid (name uuid)})))

;;
;; Event Queue
;;

(defn send-event! [user-id type & [args]]
  (let [user (db/get-user-by-id user-id)
        uid (:hash user)]
    ;; (log/debug (format "SENDING MESSAGE to user %s hash %s type %s: %s" user-id uid type args))
    (case type
      :sell-offer-match/create
      (future
        (chsk-send! uid [:sell-offer-match/create args]))

      :buy-request/create
      (future
        (chsk-send! uid [:buy-request/create args]))
      :buy-request/update
      (future
        (chsk-send! uid [:buy-request/update args]))
      :buy-request/delete
      (future
        (chsk-send! uid [:buy-request/update args]))

      :contract/create
      (future
        (chsk-send! uid [:contract/create args]))
      :contract/update
      (future
        (chsk-send! uid [:contract/update args])))))
