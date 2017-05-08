(ns oracle.bitcoin-test
  (:import [org.bitcoinj.core ECKey Sha256Hash])
  (:use clojure.test
        [clojure.java.shell :only [sh]]
        ;; ------
        oracle.bitcoin)
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            ;; -----
            [oracle.common :as common]
            [oracle.database :as db]
            [oracle.redis :as redis]
            [oracle.tasks :as tasks]
            [oracle.actions :as actions]
            [oracle.escrow :as escrow]))


(defn reset-dbs [f]
  (db/reset-database!!!)
  (redis/flush!!!)
  (tasks/populate-test-database!)
  (reset! oracle.currency/current-rates
          {:last-update 1492891183481, :rates {:usd-btc 8.01E-4, :btc-usd 1248.99}})
  (f))

(use-fixtures :once reset-dbs)

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

(deftest test-full-contract
  (with-app-wallet
    (fn [global-app global-wallet]
      (with-app-wallet
        (fn [app wallet]
          (try
            (reset! current-app global-app)
            (reset! current-wallet global-wallet)
            (tasks/workers-start!)
            ;; Create sell offer
            (db/sell-offer-set! 1 "usd" 100 200000 100)
            ;; Create buy request
            (tasks/initiate-buy-request 2 (oracle.common/currency-as-long 0.5 :btc) "usd" "btc")
            (Thread/sleep 1000)
            ;; Accept buy request
            (tasks/initiate-preemptive-task :buy-request/accept {:id 1 :transfer-info "transfer info"})
            (Thread/sleep 1500)
            ;; Contract should be created
            (is (= (:stage (db/get-contract-by-id 1)) "waiting-start"))
            ;; Start contract (client action)
            (actions/-event-msg-handler {:id :contract/start
                                         :?data {:id 1}
                                         :?reply-fn identity})
            ;; Fund the Escrow
            (wallet-send-coins app wallet
                               (:input-address (db/get-contract-by-id 1))
                               (common/btc->satoshi 0.5)
                               :us)
            (blockchain-generate 5)
            (Thread/sleep 500)
            ;; Contract should be funded
            (is (= (:escrow-amount (db/get-contract-by-id 1))
                   (common/btc->satoshi 0.5)))
            (Thread/sleep 1500)
            ;; Contract should be set to waiting transfer after the escrow has been picked up
            (is (= (:stage (db/get-contract-by-id 1)) "waiting-transfer"))
            ;; Mark out-of-band transfer as received. Also set the contract's keys as extracted
            ;; so the contract progresses
            (db/contract-update! 1 {:escrow_seller_has_key true
                                    :escrow_buyer_has_key true})
            (actions/-event-msg-handler {:id :contract/mark-transfer-received
                                         :?data {:id 1}
                                         :?reply-fn identity})
            (Thread/sleep 1500)
            (is (:transfer-received (db/get-contract-by-id 1)))
            ;; Now contract should have succeeded
            (is (= (:stage (db/get-contract-by-id 1)) "contract-success"))
            ;; Release to buyer
            ;; (let [start-coins (common/satoshi->btc (wallet-get-balance wallet))
            ;;       {:keys [escrow-script escrow-tx escrow-our-key]} (db/get-contract-by-id 1)]
            ;;   (multisig-spend global-app global-wallet
            ;;                   escrow-script escrow-tx
            ;;                   (wallet-get-current-address wallet)
            ;;                   escrow-our-key
            ;;                   (escrow/get-buyer-key 1))
            ;;   (blockchain-generate 10)
            ;;   (Thread/sleep 1000)
            ;;   ;; We should have received the coins in the wallet from the Escrow
            ;;   (is (> (wallet-get-balance wallet)
            ;;          (+ start-coins (common/btc->satoshi 0.48)))))
            (finally
              (tasks/workers-stop!)
              (reset! current-app nil)
              (reset! current-wallet nil))))))))
