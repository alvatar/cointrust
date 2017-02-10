(ns oracle.currency
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [taoensso.timbre :as log]
            [environ.core :refer [env]]
            [manifold.deferred :as d]
            [byte-streams :as bs]
            [aleph.http :as http]
            [cheshire.core :as json]
            ;; -----
            [oracle.utils :as utils]))

;;
;; Exchange rates updates task
;;

(defonce exchange-rates-worker (atom nil))
(defonce exchange-rates-worker-running? (atom false))

(defonce current-rates (atom {}))

(defn get-current-exchange-rates [] @current-rates)

(defn get-coinbase-rates []
  {:last-update (utils/unix-now)
   :rates (try
            {:usd-btc (Double/parseDouble
                       (-> @(http/get "https://api.coinbase.com/v2/exchange-rates")
                           :body
                           bs/to-string
                           (json/parse-string true)
                           :data
                           :rates
                           :BTC))
             :btc-usd (Double/parseDouble
                       (-> @(http/get "https://api.coinbase.com/v2/exchange-rates?currency=btc")
                           :body
                           bs/to-string
                           (json/parse-string true)
                           :data
                           :rates
                           :USD))}
            (catch Exception e
              (println e)
              nil))
   #_{:usd-btc (rand 1) #_0.001004, :btc-usd (rand 900) #_995.97}
   })

(defn make-exchange-rates-worker []
  (future
    (try
      (loop []
        (when @exchange-rates-worker-running?
          (log/debug "Updating exchange rates")
          (swap! current-rates #(or (get-coinbase-rates) %))
          (Thread/sleep 60000)
          (recur)))
      (catch Exception e
        (println e)
        e))))

(defn start-exchange-rates-updates! []
  (when-not @exchange-rates-worker
    (reset! exchange-rates-worker-running? true)
    (reset! exchange-rates-worker (make-exchange-rates-worker))
    (log/debug "Exchange rates updater started")
    'started))

(defn stop-exchange-rates-updates! []
  (when @exchange-rates-worker
    (reset! exchange-rates-worker-running? false)
    (reset! exchange-rates-worker nil)
    (log/debug "Exchange rates updater stopped")
    'stopped))