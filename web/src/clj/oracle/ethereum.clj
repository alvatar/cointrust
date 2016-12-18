(ns oracle.ethereum
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [cheshire.core :as json]
            [aleph.http :as http]
            [manifold.deferred :as d]
            [byte-streams :as bs]))


(def req-counter (atom 0))

(defn to-hex [d] (str "0x" (.toString (biginteger d) 16)))

(defn from-hex [hex] (BigInteger. hex 16))

(defn wei->eth [wei] (/ wei 1000000000000000000.))

(defn send-command [method & [params extra]]
  (-> @(http/post "http://localhost:8545"
                  {:body (json/generate-string (merge {:jsonrpc "2.0"
                                                       :method method
                                                       :params (or params [])
                                                       :id (swap! req-counter inc)}
                                                      extra))})
      :body
      bs/to-string
      json/parse-string
      ;(get "result")
      ))

(defn eth-accounts []
  (send-command "eth_accounts"))

(defn eth-get-balance [account & [block]]
  (from-hex (subs (send-command "eth_getBalance" [account (or block "latest")]) 2)))
