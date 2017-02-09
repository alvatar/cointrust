(ns oracle.worker
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.tasks :as tasks]
            [oracle.bitcoin :as bitcoin])
  (:gen-class))


(defn start! []
  (tasks/workers-start!))

(defn stop! []
  (tasks/workers-stop!))

(defn -main [] (start!))
