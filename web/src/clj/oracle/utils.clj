(ns oracle.utils
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [clj-time.core :as time]
            [clj-time.coerce :as time-coerce]
            [clj-time.format :as timef]))


;;
;; Utils
;;

(defn unix-now [] (time-coerce/to-long (time/now)))

(defn unix-after [reference duration] (time-coerce/to-long (time/plus reference duration)))

(def human-formatter (timef/formatter "dd-MMMMMMMMMM-yyyy - HH:mm"))

(defn human-now [] (timef/unparse human-formatter (time/now)))

;;
;; Human ID generator
;;

(defn- word-list [part]
  (clojure.string/split
   (slurp (io/resource (str "public/" part ".txt")))
   #"\n"))
(def adjectives (word-list "adjectives"))
(def nouns (word-list "nouns"))
(def verbs (word-list "verbs"))
(def adverbs (word-list "nouns"))

(defn human-id-generator []
  (clojure.string/join " " [(rand-nth adjectives)
                            (rand-nth nouns)
                            (rand-nth verbs)
                            (rand-nth nouns)]))
