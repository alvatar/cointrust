(ns oracle.redis
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            [cheshire.core :as json]))


;;
;; Redis
;;

(def redis-url (or (env :redis-url) "redis://127.0.0.1:6379"))

(log/debugf "Connecting to Redis at %s\n" redis-url)

(def redis-conn {:pool {}
                 :spec {:uri redis-url}})

(defmacro wcar* [& body] `(r/wcar redis-conn ~@body))

(defn redis->json [stored]
  (into {} (for [[k v] (partition 2 stored)] [(keyword (str k)) (json/parse-string v true)])))

(defn flush!!! [] (wcar* (r/flushall)))
