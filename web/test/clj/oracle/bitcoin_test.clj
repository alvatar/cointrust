(ns oracle.bitcoin-test
  (:use clojure.test
        [clojure.java.shell :only [sh]]
        oracle.bitcoin)
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [oracle.common :as common]))


(defn blockchain-generate [n]
  (sh "bitcoin-cli" "-regtest" "generate" (str n)))

(defn wallet-fund [w btc-amount]
  (sh "bitcoin-cli" "-regtest" "sendtoaddress"
      (wallet-get-current-address w) (str btc-amount)))

(defn print-wallet-funds [w]
  (log/debugf "Wallet funds: %s" (wallet-get-balance w)))

(defn with-app [f]
  (let [spvfile (str "spvfile-test-" (rand-int 99999))]
    (try
      (let [app (make-app spvfile)
            wallet (make-wallet app)
            listeners (wallet-add-listeners! app wallet)]
        (app-add-wallet app wallet)
        (app-start! app true)
        (wallet-fund wallet 1.1)
        (blockchain-generate 10)
        (Thread/sleep 1000)
        (f app wallet)
        (.stop (:peergroup app))
        (wallet-remove-listeners! wallet listeners))
      (finally
        (sh "rm" spvfile)))))

(deftest create-escrow
  (with-app
    (fn [app wallet]
      (println "HELLO " wallet))))
