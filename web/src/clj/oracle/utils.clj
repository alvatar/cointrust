(ns oracle.utils
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [clj-time.core :as time]
            [clj-time.coerce :as time-coerce]))


;;
;; Utils
;;

(defn unix-now [] (time-coerce/to-long (time/now)))

(defn unix-after [reference duration] (time-coerce/to-long (time/plus reference duration)))
