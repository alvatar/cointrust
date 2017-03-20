(ns oracle.utils
  (:require [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]))

;;
;; Utils
;;

(defn json->clj [s] (js->clj (.parse js/JSON s)))

(defn clj->json [ds] (.stringify js/JSON (clj->js ds)))

(defn log* [& args] (when true (js/console.log (clojure.string/join " " (map str args)))))

(defn open-page [url blank?]
  (if blank?
    (. js/window open url "_blank")
    (aset js/window "location" url)))

(defn find-in [col id] (first (keep-indexed #(when (= (:id %2) id) %1) col)))

(defn some-update [predicate f coll]
  ((cond (vector? coll) mapv
         :else map)
   (fn [x] (if (predicate x) (f x) x))
   coll))
