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
    (store-notification uid notif-uuid {:title title :message message})
    (chsk-send! uid [:notification/create {:uuid notif-uuid :title title :message message}])))

(defn get-notifications [uid]
  (for [[uuid notif] (redis->json (wcar* (r/hgetall (str "user->notifications:" uid))))]
    (merge notif {:uuid (name uuid)})))

;;
;; Dispatcher
;;

(defn dispatch! [user-id type & [args]]
  ;;(log/debug (format "SENDING MESSAGE %s: %s" type args))
  (let [uid (db/get-user-by-id user-id)]
    (case type
      :buy-request-created
      (future
        (chsk-send! uid [:buy-request/create args])
        (notification uid "Buy request created"
                      "The system will look now for a trusted partner for this transaction."))
      :buy-request-matched
      (future
        (chsk-send! uid [:buy-request/match args])
        (notification uid "Partner found"
                      "We found a trusted partner for your transaction. We are now waiting for its response."))
      :sell-offer-matched
      (future
        (chsk-send! uid [:sell-offer/match args]))
      :buy-request-restart
      (future
        (chsk-send! uid [:buy-request/restart args])
        (notification uid "Buy request restarted"
                      "Your transaction partner is not reponding without the provided time. We are looking for a new partner for you."))
      :buy-request-accept
      (future
        (chsk-send! uid [:buy-request/accepted args])
        (notification uid "Buy request accepted"
                      "A contract will be created briefly. You will be notified when the seller funds the Escrow account for the contract."))
      :buy-request-decline
      (future
        (chsk-send! uid [:buy-request/declined args])
        (notification uid "Buy request declined"
                      "We regret to inform you that the assigned counterparty has failed to correctly initiate the contract. We are looking for a new transaction partner."))
      :contract-create
      (future
        (chsk-send! uid [:contract/create args]))
      :contract-escrow-funded
      (future
        (chsk-send! uid [:contract/escrow-funded args])
        (notification uid "Escrow funds received"
                      (if (= (:seller-id args) user-id)
                        "We've successfully received your funds in the Escrow account"
                        "The seller has successfully funded the Escrow account")))
      ;; :contract-waiting-transfer
      ;; (future
      ;;   (chsk-send! uid [:contract/waiting-transfer args]))
      :contract-mark-transfer-sent-ack
      (future
        (chsk-send! uid [:contract/mark-transfer-sent-ack args])
        (notification uid "Transfer sent"
                      (if (= (:seller-id args) user-id)
                        "The buyer has notified the system of an initiated transfer of funds. We will wait for your confirmation."
                        "The seller has been notified of the transfer sent.")))
      :contract-mark-transfer-received-ack
      (future
        (chsk-send! uid [:contract/mark-transfer-received-ack args])
        (notification uid "Transfer received"
                      (if (= (:seller-id args) user-id)
                        "Thanks for confirming that you've successfully received the transfer."
                        "The seller has confirmed the reception of the funds.")))
      :contract-holding-period
      (future
        (chsk-send! uid [:contract/holding-period args])
        (notification uid "Holding period"
                      "The contract has achieved it's final stage. For buyer and seller protection, the escrow account will hold the cryptocurrency for 100 days."))
      :contract-success
      (future
        (chsk-send! uid [:contract/success args])
        (notification uid "Contract success!"
                      (if (= (:seller-id args) user-id)
                        "The contract has correctly finalized"
                        "The contract has correctly finalized. You can now withdraw your cryptocurrency from the escrow account!")))
      :contract-broken
      (future
        (chsk-send! uid [:contract/broken args])
        (notification uid "Contract broken"
                      "The contract was broken. We will proceed accordingly.")))))
