(ns oracle.events
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.database :as db]))

(defn init! [chsk-send!_]
  (def chsk-send! chsk-send!_))

;;
;; TODO: notifications should be acknowledged and stored otherwise
;;

(defn dispatch! [user-id type & [args]]
  ;;(log/debug (format "SENDING MESSAGE %s: %s" type args))
  (let [uid (db/get-user-by-id user-id)]
    (case type
      :buy-request-created
      (future
        (chsk-send! uid [:buy-request/create args])
        (chsk-send! uid [:notification/create
                         {:title "Buy request created"
                          :message "The system will look now for a trusted partner for this transaction."}]))
      :buy-request-matched
      (future
        (chsk-send! uid [:buy-request/match args])
        (chsk-send! uid [:notification/create
                         {:title "Partner found"
                          :message "We found a trusted partner for your transaction. We are now waiting for its response."}]))
      :sell-offer-matched
      (future
        (chsk-send! uid [:sell-offer/match args]))
      :buy-request-restart
      (future
        (chsk-send! uid [:buy-request/restart args])
        (chsk-send! uid [:notification/create
                         {:title "Buy request restarted"
                          :message "Your transaction partner is not reponding without the provided time. We are looking for a new partner for you."}]))
      :buy-request-accept
      (future
        (chsk-send! uid [:buy-request/accepted args])
        (chsk-send! uid [:notification/create
                         {:title "Buy request accepted"
                          :message "A contract will be created briefly. You will be notified when the seller funds the Escrow account for the contract."}]))
      :buy-request-decline
      (future
        (chsk-send! uid [:buy-request/declined args])
        (chsk-send! uid [:notification/create
                         {:title "Buy request declined"
                          :message "We regret to inform you that the assigned counterparty has failed to correctly initiate the contract. We are looking for a new transaction partner."}]))
      :contract-create
      (future
        (chsk-send! uid [:contract/create args]))
      :contract-waiting-transfer
      (future
        (chsk-send! uid [:contract/waiting-transfer args])
        (chsk-send! uid [:notification/create
                         {:title "Waiting transfer"
                          :message (str "The seller is waiting for your transfer. Please proceed. When done, click the action button. The following are the required details for the transfer:\n"
                                        (:transfer-info args))}]))
      :contract-mark-transfer-sent-ack
      (future
        (chsk-send! uid [:contract/mark-transfer-sent-ack args])
        (chsk-send! uid [:notification/create
                         {:title "Transfer sent"
                          :message (if (= (:seller-id args) user-id)
                                     "The buyer has notified the system of an initiated transfer of funds. We will wait for your confirmation."
                                     "The seller has been notified of the transfer sent.")}]))
      :contract-mark-transfer-received-ack
      (future
        (chsk-send! uid [:contract/mark-transfer-received-ack args])
        (chsk-send! uid [:notification/create
                         {:title "Transfer received"
                          :message "The seller has confirmed the reception of the funds."}]))
      :contract-holding-period
      (future
        (chsk-send! uid [:contract/holding-period args])
        (chsk-send! uid [:notification/create
                         {:title "Holding period"
                          :message "The contract has achieved it's final stage. For buyer and seller protection, the escrow account will hold the cryptocurrency for 100 days."}]))
      :contract-success
      (future
        (chsk-send! uid [:contract/success args])
        (chsk-send! uid [:notification/create
                         {:title "Contract success!"
                          :message (if (= (:seller-id args) user-id)
                                     "The contract has correctly finalized"
                                     "The contract has correctly finalized. You can now withdraw your cryptocurrency from the escrow account!")}]))
      :contract-broken
      (future
        (chsk-send! uid [:contract/broken args])
        (chsk-send! uid [:notification/create
                         {:title "Contract broken"
                          :message "The contract was broken. We will proceed accordingly."}])))))
