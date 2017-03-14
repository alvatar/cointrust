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

(defn add-event! [user-id type & [args]]
  (let [user (db/get-user-by-id user-id)
        uid (:hash user)]
    ;; (log/debug (format "SENDING MESSAGE to user %s hash %s type %s: %s" user-id uid type args))
    (case type
      :buy-request-created
      (future
        (chsk-send! uid [:buy-request/create args])
        (comment (notification uid "Buy request created. Looking for a trusted partner." "")))
      :buy-request-matched
      (future
        (chsk-send! uid [:buy-request/match args])
        (comment (notification uid "Partner found. Waiting response." "")))
      :sell-offer-matched
      (future
        (chsk-send! uid [:sell-offer/match args]))
      :buy-request-accept
      (future
        (chsk-send! uid [:buy-request/accepted args])
        (comment (notification uid "Buy request accepted." "")))
      :buy-request-decline
      (future
        (chsk-send! uid [:buy-request/declined args])
        (comment (notification uid "Buy request declined." "")))
      :buy-request-timed-out
      (future
        (chsk-send! uid [:buy-request/timed-out args])
        (comment (notification uid "Buy request timed out." "")))
      :contract-create
      (future
        (chsk-send! uid [:contract/create args]))
      :contract-escrow-funded
      (future
        (chsk-send! uid [:contract/escrow-funded args])
        (comment (notification uid "Escrow funds received." "")))
      :contract-mark-transfer-received-ack
      (future
        (chsk-send! uid [:contract/mark-transfer-received-ack args])
        (comment (notification uid "Transfer received." "")))
      :contract-success
      (future
        (chsk-send! uid [:contract/success args])
        (comment (notification uid "Contract executed successfully." "")))
      :contract-broken
      (future
        (chsk-send! uid [:contract/broken args])
        (comment (notification uid "Contract broken." "")))
      :contract-escrow-release-success
      (future
        (chsk-send! uid [:contract/escrow-release-success args])
        (comment (notification uid "Escrow Released." "")))
      :contract-escrow-release-failure
      (future
        (chsk-send! uid [:contract/escrow-release-failure args])
        (comment (notification uid "Failure Releasing Escrow." "")))
      :contract-escrow-insufficient
      (future
        (chsk-send! uid [:contract/escrow-insufficient args])
        (comment (notification uid "Error: Received insufficient funds"))))))
