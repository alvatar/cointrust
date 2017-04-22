(ns oracle.bitcoin-test
  (:import [org.bitcoinj.core ECKey Sha256Hash])
  (:use clojure.test
        [clojure.java.shell :only [sh]]
        oracle.bitcoin)
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [oracle.common :as common]))


;;
;; Utils
;;

(defn blockchain-generate [n]
  (let [com (sh "bitcoin-cli" "-regtest" "generate" (str n))]
    (when-not (zero? (:exit com)) (println com))))

(defn wallet-fund [w btc-amount]
  (let [com (sh "bitcoin-cli" "-regtest" "sendtoaddress"
                (wallet-get-current-address w)
                (str btc-amount))]
    (when-not (zero? (:exit com)) (println com))
    (:out com)))

(defn with-app-wallet [f]
  (let [spvfile (str "spvchain-test-" (rand-int 99999))]
    (try
      (let [app (make-app spvfile)
            wallet (make-wallet app)
            listeners (wallet-add-listeners! app wallet)]
        (app-add-wallet app wallet)
        (app-start! app true true)
        ;; Note: do not fund with more, since it will produce a "too large error"
        (let [input-tx (clojure.string/trim (wallet-fund wallet 1))]
          (blockchain-generate 1)
          (Thread/sleep 500)
          (f app wallet))
        (app-stop! app)
        (wallet-remove-listeners! wallet listeners))
      (finally
        (sh "rm" spvfile)))))

;;
;; Tests
;;

(deftest test-wallet-send-coins
  (with-app-wallet
    (fn [app1 wallet1]
      (with-app-wallet
        (fn [app2 wallet2]
          (wallet-send-coins app1 wallet1 (wallet-get-current-address wallet2)
                             (common/btc->satoshi 0.1)
                             :us)
          (blockchain-generate 1)
          (Thread/sleep 500)
          (is (>= (common/satoshi->btc (wallet-get-balance wallet2)) 1.1))
          (is (<= (common/satoshi->btc (wallet-get-balance wallet1)) 0.9)))))))

(deftest test-escrow-create-and-spend
  (with-app-wallet
    (fn [app wallet]
      (let [our-key-bytes (.getPrivKeyBytes (ECKey.))
            seller-key-bytes (.getPrivKeyBytes (ECKey.))
            buyer-key-bytes (.getPrivKeyBytes (ECKey.))
            {:keys [escrow-tx escrow-script]}
            (create-multisig app wallet (common/btc->satoshi 0.1) our-key-bytes seller-key-bytes buyer-key-bytes)]
        (blockchain-generate 1)
        (is (< (common/satoshi->btc (wallet-get-balance wallet)) 0.91))
        (multisig-spend app wallet escrow-script escrow-tx (wallet-get-current-address wallet) our-key-bytes seller-key-bytes)
        (blockchain-generate 1)
        (is (> (common/satoshi->btc (wallet-get-balance wallet)) 0.99))))))
