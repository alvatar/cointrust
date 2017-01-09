(ns oracle.events
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.database :as db]))

(defn init! [chsk-send!_]
  (def chsk-send! chsk-send!_))

(defn dispatch! [user-id type & [args]]
  (log/debug (format "SENDING MESSAGE %s: %s" type args))
  (let [uid (db/get-user-by-id user-id)]
   (case type
     :buy-request-created
     (future
       (chsk-send! uid [:buy-request/created args])
       (chsk-send! uid [:notification/create
                        {:title "Buy request created"
                         :message "The system will look now for a trusted partner for this transaction."}]))
     :buy-request-matched
     (future
       (chsk-send! uid [:buy-request/matched args])
       (chsk-send! uid [:notification/create
                        {:title "Partner found"
                         :message "We found a trusted partner for your transaction. We are now waiting for its response."}]))
     :sell-offer-matched
     (future
       (chsk-send! uid [:sell-offer/matched args]))
     :buy-request-restarted
     (future
       (chsk-send! uid [:buy-request/restarted args])
       (chsk-send! uid [:notification/create
                        {:title "Buy request restarted"
                         :message "Your transaction partner is not reponding without the provided time. We are looking for a new partner for you."}]))
     :buy-request-accepted
     (future
       (chsk-send! uid [:buy-request/accepted args])
       (chsk-send! uid [:notification/create
                        {:title "Buy request accepted"
                         :message "A contract will be created briefly. You will be notified when the seller funds the Escrow account for the contract."}]))
     :buy-request-declined
     ;; TODO
     (throw (Exception. "NOT IMPLEMENTED"))
     #_(future
       (chsk-send! uid [:sell-offer/declined args])))))
