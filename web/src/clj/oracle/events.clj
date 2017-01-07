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
     (do (chsk-send! uid [:buy-request/created args])
         (chsk-send! uid [:notification/create {:title "Buy request created" :message "The system will look now for a trusted partner for this transaction."}]))
     :buy-request-matched
     (do (chsk-send! uid [:buy-request/matched args])
         (chsk-send! uid [:notification/create {:title "Partner found" :message "We found a trusted partner for your transaction. We are now waiting for its response."}])))))
