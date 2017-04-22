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

(defn with-app-wallet [f]
  (let [spvfile (str "spvfile-test-" (rand-int 99999))]
    (try
      (let [app (make-app spvfile)
            wallet (make-wallet app)
            listeners (wallet-add-listeners! app wallet)]
        (app-add-wallet app wallet)
        (app-start! app true true)
        (blockchain-generate 10)
        (wallet-fund wallet 10)
        (blockchain-generate 10)
        (f app wallet)
        (.stop (:peergroup app))
        (wallet-remove-listeners! wallet listeners))
      (finally
        (sh "rm" spvfile)))))

(deftest wallet-send-money
  (with-app-wallet
    (fn [app1 wallet1]
      (with-app-wallet
        (fn [app2 wallet2]
          (wallet-send-coins app1 wallet1 (wallet-get-current-address wallet2)
                             (common/btc->satoshi 1)
                             :us)
          (blockchain-generate 40)
          ;;(println (common/satoshi->btc (wallet-get-balance wallet1)))
          ;;(println (common/satoshi->btc (wallet-get-balance wallet2)))
          (is (> (common/satoshi->btc (wallet-get-balance wallet2)) 10))
          (is (< (common/satoshi->btc (wallet-get-balance wallet1)) 10)))))))

