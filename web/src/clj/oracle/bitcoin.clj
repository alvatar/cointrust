(ns oracle.bitcoin
  (:require [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]])
  (:import [java.io File]
           (java.net InetAddress)
           [com.google.common.collect ImmutableList]
           [org.bitcoinj.core Address BlockChain Coin Context ECKey PeerAddress PeerGroup Transaction
            TransactionBroadcast]
           [org.bitcoinj.core.listeners DownloadProgressTracker]
           [org.bitcoinj.kits WalletAppKit]
           [org.bitcoinj.net.discovery DnsDiscovery]
           [org.bitcoinj.params MainNetParams TestNet3Params RegTestParams]
           [org.bitcoinj.script ScriptBuilder]
           [org.bitcoinj.store MemoryBlockStore SPVBlockStore]
           [org.bitcoinj.utils BriefLogFormatter]
           [org.bitcoinj.wallet SendRequest Wallet]))


;;
;; Utils
;;

(defn satoshi->btc [sat]
  (/ sat 100000000))

(defn btc->satoshi [btc]
  (* btc 100000000))

(defn substract-satoshi-fee [sat]
  (- sat 5000))

(defn substract-btc-fee [sat]
  (- sat 0.00005))

;;
;; App
;;

(defrecord App [network-params blockchain peergroup wallets])

(defn make-app []
  (let [network-params (case (env :env)
                         "production" (. MainNetParams get)
                         "staging" (. TestNet3Params get)
                         (. RegTestParams get))
        blockchain (BlockChain. network-params
                                ;;(MemoryBlockStore.)
                                (SPVBlockStore. network-params (File. ".spvchain")))]
    (. BriefLogFormatter init)
    (App. network-params blockchain (PeerGroup. network-params blockchain) [])))

(defn app-start! [app]
  (let [listener (proxy [DownloadProgressTracker] []
                   (doneDownload []
                     (println "Blockchain download complete"))
                   (startDownload [blocks]
                     (println (format "Downloading %d blocks" blocks)))
                   (progress [pct block-so-far date]
                     (println pct)))]
    ((case (env :env)
       ("production" "staging") (fn [pg] (.addPeerDiscovery (DnsDiscovery. pg (:network-params app))))
       (fn [pg]
         (.addAddress pg (PeerAddress. (. InetAddress getLocalHost) (.getPort (:network-params app))))
         (.setMaxConnections pg 1))) (:peergroup app))
    (doto (:peergroup app)
      (.start)
      (.startBlockChainDownload listener))
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "Shutting down Bitcoin app")
                                 (. Context propagate (:network-params app))
                                 (.stop (:peergroup app))
                                 ;; TODO: Save wallets to their Files
                                 (.close (.getBlockStore (:blockchain app))))))
    listener))

(defn app-add-wallet [app wallet]
  (.addWallet (:blockchain app) wallet)
  (.addWallet (:peergroup app) wallet)
  (update app :wallets #(conj % wallet)))

;;
;; Wallet
;;

(defn make-wallet [app]
  (Wallet. (:network-params app)))

(defn wallet-get-current-address [wallet]
  (.currentReceiveAddress wallet))

(defn wallet-get-balance [wallet]
  (.getValue (.getBalance wallet)))

(defn wallet-send-coins [wallet app target-address amount]
  (.sendCoins wallet
              (:peergroup app)
              (. Address fromBase58 (.getNetworkParameters wallet) target-address)
              (. Coin valueOf amount)))

(defn wallet-send-all-funds-to [wallet app target-address]
  (wallet-send-coins wallet app target-address
                     (substract-satoshi-fee (wallet-get-balance wallet))))

;;
;; Multisig
;;

(defrecord MultiSig [our-key seller-key buyer-key contract script])

;; (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))
;; (. org.bitcoinj.core.Base58 encode (.getSecretBytes (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))))
;; Private key: "GMPBVAgMHJ6jUk7g7zoYhQep5EgpLAFbrJ4BbfxCr9pp"
;; Address: "mjd3Ug6t53rbsrZhZpCdobqHDUnU8sjAYd"

(defn create-multisig [app]
  (let [our-key (ECKey.)
        ;;our-key (. wallet currentReceiveKey)
        ;;  We can get the other parties public key from somewhere
        ;; ECKey serverKey = new ECKey(null, publicKeyBytes);
        seller-key (ECKey.)
        buyer-key (ECKey.)
        keys (. ImmutableList of our-key seller-key buyer-key)
        ;; Prepare a template for the contract.
        contract (Transaction. (:network-params app))
        script (. ScriptBuilder createMultiSigOutputScript 2 keys)]
    (MultiSig. our-key seller-key buyer-key contract script)))

;; (defn multisig-get-our-address [multisig network-params]
;;   (.toString (.toAddress (:our-key multisig) network-params)))

(defn multisig-setup-from-wallet [multisig wallet app]
  (let [wallet-tx (first (.getUnspents wallet))
        coins (.getBalance wallet) ;;(. Coin valueOf 0 0001)
        output (.addOutput (:contract multisig) coins (:script multisig))
        input (.addInput (:contract multisig) wallet-tx)]
    (let [request (. SendRequest forTx (:contract multisig))]
      (try (.completeTx wallet request)
           ;;(. (:peergroup app) broadcastTransaction (.tx request))
           (.broadcastTransaction (:peergroup app) (.tx request))
           (catch Exception e (println e))))))

(defn multisig-send-ours [multisig app target-address]
  (let [multisig-output (.getOutput (:contract multisig) 0)
        ;;multisig-script (.getScriptPubKey transaction-output)
        value (.getValue multisig-output)]
    ;;(.isSentToMultiSig (:script multisig))
    (let [spend-tx (Transaction. (:network-params app))]
      (.addOutput spend-tx value (. Address fromBase58 (:network-params app) target-address))
      (.addInput spend-tx multisig-output)
      (.calculateSignature spend-tx
                           0
                           (:our-key multisig)
                           (:script multisig)
                           (org.bitcoinj.core.Transaction$SigHash/ALL)
                           false))))

(defn multisig-send-other [multisig app target-address our-signature other-key]
  (let [multisig-output (.getOutput (:contract multisig) 0)
        value (.getValue multisig-output)
        spend-tx (Transaction. (:network-params app))]
    (.addOutput spend-tx value (. Address fromBase58 (:network-params app) target-address))
    (let [input (.addInput spend-tx multisig-output)
          other-signature (.calculateSignature spend-tx
                                               0
                                               other-key
                                               (:script multisig)
                                               (org.bitcoinj.core.Transaction$SigHash/ALL)
                                               false)
          spend-script (. ScriptBuilder
                          createMultiSigInputScript
                          (. ImmutableList of other-signature our-signature))]
      (.setScriptSig input spend-script)
      (.verify input multisig-output)
      (.broadcastTransaction (:peergroup app) spend-tx)
      [spend-tx spend-script])))

(comment
  (def app (make-app))
  (app-start! app)
  (def w (make-wallet app))
  (def app (app-add-wallet app w))
  (def multisig (create-multisig app))
  ;;(def multisig-address (multisig-get-our-address multisig app))
  (def mtx (multisig-setup-from-wallet multisig w app))
  ;;(wallet-send-coins (get-current-wallet) (get-current-peergroup) multisig-address 10000)
  #_(multisig-send-other multisig
                       (get-current-peergroup)
                       (get-current-network-params)
                       "n2eMqTT929pb1RDNuqEnxdaLau1rxy3efi"
                       txsig
                       (:seller-key multisig)))

(def tx-broadcast-progress-cb
  (reify org.bitcoinj.core.TransactionBroadcast$ProgressCallback
    (onBroadcastProgress [this p] (println p))))
;;

