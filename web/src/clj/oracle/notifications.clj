(ns oracle.notifications
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.database :as db]))

(defn init! [chsk-send!_]
  (def chsk-send! chsk-send!_))

;; TODO cache user-id hashes
(defn send! [user-id type & [args]]
  (log/debug (format "SENDING MESSAGE %s: %s" type args))
  (case type
    :buy-request-created
    (chsk-send! (db/get-user-by-id user-id) [:buy-request/created args])
    :buy-request-matched
    (chsk-send! (db/get-user-by-id user-id) [:buy-request/matched args])))
