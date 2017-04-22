(ns oracle.bitcoin
  (:import [java.io File ByteArrayOutputStream ByteArrayInputStream]
           [java.net InetAddress]
           [java.util ArrayList]
           [com.google.common.collect ImmutableList]
           [org.bitcoinj.core Address BlockChain Coin Context ECKey PeerAddress PeerGroup Transaction
            TransactionBroadcast Sha256Hash]
           [org.bitcoinj.core.listeners DownloadProgressTracker]
           [org.bitcoinj.kits WalletAppKit]
           [org.bitcoinj.net.discovery DnsDiscovery]
           [org.bitcoinj.params MainNetParams TestNet3Params RegTestParams]
           [org.bitcoinj.script Script ScriptBuilder]
           [org.bitcoinj.store MemoryBlockStore SPVBlockStore PostgresFullPrunedBlockStore]
           [org.bitcoinj.utils BriefLogFormatter]
           [org.bitcoinj.wallet SendRequest Wallet])
  (:require [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [>! <! >!! <!! chan]]
            [cemerick.url :refer [url]]
            [cheshire.core :as json]
            [taoensso.carmine :as r]
            [taoensso.encore :as encore]
            ;; -----
            [oracle.common :as common]
            [oracle.redis :as redis]
            [oracle.database :as db]
            [oracle.escrow :as escrow]))


(declare create-multisig)
(declare wallet-serialize)

;;
;; Utils
;;

(defn make-tx-broadcast-progress-cb []
  (reify org.bitcoinj.core.TransactionBroadcast$ProgressCallback
    (onBroadcastProgress [this p] (println p))))

(defn make-private-key [] (.getPrivKeyBytes (ECKey.)))

(defn make-address [app s]
  (try (. Address fromBase58 (:network-params app) s)
       (catch Exception e)))

;;
;; Globals
;;

(defonce current-app (atom nil))
(defonce current-wallet (atom nil))
(defonce current-listeners (atom nil))

;;
;; App
;;

(defrecord App [network-params blockchain peergroup wallets])

(defn make-app [& [spvchain-file]]
  (let [network-params (case (env :env)
                         "production" (. MainNetParams get)
                         "staging" (. TestNet3Params get)
                         (. RegTestParams get))
        uri (url (str "http" (subs (or (env :database-url) "postgres://alvatar:@localhost:5432/oracledev") 8)))
        blockchain (BlockChain. network-params
                                ;;(MemoryBlockStore. network-params)
                                (SPVBlockStore. network-params (File. (or spvchain-file ".spvchain")))
                                ;; According to the documentation 1000 blocks stored is safe
                                #_(PostgresFullPrunedBlockStore.
                                   network-params 1000
                                   (format "%s:%s" (:host uri) (:port uri))
                                   (subs (:path uri) 1)
                                   (:username uri)
                                   (:password uri)))]
    (. BriefLogFormatter init)
    (App. network-params blockchain (PeerGroup. network-params blockchain) [])))

(defn app-stop! [app]
  (.stop (:peergroup app))
  (.close (.getBlockStore (:blockchain app))))

(defn app-start! [app & [block? silent?]]
  (let [is-done (chan)
        listener (proxy [DownloadProgressTracker] []
                   (doneDownload []
                     (println "Blockchain download complete")
                     (when block? (>!! is-done true)))
                   (startDownload [blocks]
                     (when-not silent? (println (format "Downloading %d blocks" blocks))))
                   (progress [pct block-so-far date]
                     (when-not silent? (println pct))))]
    (case (env :env)
      ("production" "staging")
      (.addPeerDiscovery (:peergroup app) (DnsDiscovery. (:network-params app)))
      (doto (:peergroup app)
        (.addAddress (PeerAddress. (. InetAddress getLocalHost) (.getPort (:network-params app))))
        (.setMaxConnections 1)))
    (doto (:peergroup app)
      (.start)
      (.startBlockChainDownload listener))
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "Shutting down Bitcoin app")
                                 (app-stop! app))))
    (when block? (<!! is-done))))

(defn app-add-wallet [app wallet]
  (.addWallet (:blockchain app) wallet)
  (.addWallet (:peergroup app) wallet)
  (update app :wallets #(conj % wallet)))

;;
;; Wallet
;;

(defn wallet-serialize [wallet]
  (let [bs (ByteArrayOutputStream.)]
    (.saveToFileStream wallet bs)
    (.toByteArray bs)))

(defn wallet-deserialize [sr]
  (. Wallet loadFromFileStream (ByteArrayInputStream. sr) nil))

(defn btc-log! [& m]
  (let [formatted (apply format m)]
    (log/debug formatted)
    (db/log! "info" "bitcoin" formatted)))

(defn log-action-required! [m] (db/log! "action-required" "bitcoin" m))

(defn make-wallet [app] (Wallet. (:network-params app)))

(defn wallet-get-current-address [wallet]
  (.toString (.currentReceiveAddress wallet)))

(defn wallet-get-fresh-address [wallet]
  (let [addr (.freshReceiveAddress wallet)]
    (.addWatchedAddress wallet addr)
    (db/save-current-wallet (wallet-serialize wallet))
    (.toString addr)))

(defn wallet-get-balance [wallet]
  (.getValue (.getBalance wallet)))

(defn wallet-send-coins [app wallet address amount pays-fees]
  (let [amount (case pays-fees
                 :us amount
                 ;; TODO: proper fee calculation
                 :them (- amount
                          (if (= (env :env) "production")
                            70000 ; http://bitcoinexchangerate.org/fees
                            100000)))
        send-result (.sendCoins wallet
                                (:peergroup app)
                                (. Address fromBase58 (.getNetworkParameters wallet) address)
                                (. Coin valueOf amount))]
    (db/save-current-wallet (wallet-serialize wallet))
    (.get (.-broadcastComplete send-result))
    send-result))

(defn wallet-release-contract-coins [app wallet contract-id target-address amount pays-fees]
  (try (let [send-result (app wallet-send-coins app wallet target-address amount pays-fees)
             tx (.-tx send-result)
             tx-hash (.. tx getHash toString)]
         (log/debugf "Send payment for contract %s of %s BTC to address %s transaction %s, fees paid by %s\n"
                     contract-id (common/satoshi->btc amount) target-address tx-hash pays-fees)
         tx-hash)
       (catch Exception e
         (log/debugf "Error sending coins: %s" e) false)))

(defn follow-transaction! [tx-hash address amount contract-id]
  (redis/wcar* (r/hset "pending-transactions" tx-hash
                       (json/generate-string
                        {:address address
                         :amount amount
                         :contract-id contract-id}))))

(defn unfollow-transaction! [tx-hash]
  (redis/wcar* (r/hdel "pending-transactions" tx-hash)))

(defn followed-transaction [tx-hash]
  (when-let [tx (redis/wcar* (r/hget "pending-transactions" tx-hash))]
    (json/parse-string tx true)))

(defn make-coins-received-listener [app]
  (reify org.bitcoinj.wallet.listeners.WalletCoinsReceivedEventListener
    (onCoinsReceived [this wallet transaction prev-balance new-balance]
      (try ;;(log/debugf "BITCOIN *** Coins received: %s" transaction)
        (db/save-current-wallet (wallet-serialize wallet))
        (doseq [output (.getOutputs transaction)]
          (when-let [address-p2pk (.getAddressFromP2PKHScript output (:network-params app))]
            (let [tx-hash (.. transaction getHash toString)
                  address-payed (.toString address-p2pk)
                  amount-payed (.getValue (.getValue output))]
              (when-let [contract (db/get-contract-by-input-address address-payed)]
                (log/infof "BITCOIN *** Following payment of %s BTC to %s funds contract ID %s with amount %s in transaction %s"
                           (common/satoshi->btc amount-payed) address-payed (:id contract) (common/satoshi->btc amount-payed) tx-hash)
                (follow-transaction! (.getHashAsString transaction) address-payed amount-payed (:id contract))))))
        (catch Exception e (log/error e))))))

(def transaction-confidence:building (.getValue org.bitcoinj.core.TransactionConfidence$ConfidenceType/BUILDING))
(def transaction-confidence:dead (.getValue org.bitcoinj.core.TransactionConfidence$ConfidenceType/DEAD))
(def transaction-confidence:in-conflict (.getValue org.bitcoinj.core.TransactionConfidence$ConfidenceType/IN_CONFLICT))

(defn make-transaction-confidence-listener [app wallet]
  (reify org.bitcoinj.core.listeners.TransactionConfidenceEventListener
    (onTransactionConfidenceChanged [this wallet transaction]
      (try
        (let [tx-hash (.getHashAsString transaction)]
          (when-let [tx-data (followed-transaction tx-hash)]
            (let [confidence (.getConfidence transaction)
                  tx-confidence-type (.getConfidenceType confidence)
                  tx-confidence-val (.getValue tx-confidence-type)
                  tx-depth (.getDepthInBlocks confidence)
                  address-payed (:address tx-data)
                  contract-id (:contract-id tx-data)]
              (log/debugf "BITCOIN *** Transaction %s changed to: %s with depth %s confidence %s"
                          tx-hash (.getConfidence transaction) tx-depth tx-confidence-type)
              (cond (>= tx-depth 1)
                    (let [amount (:amount tx-data)
                          contract (db/get-contract-by-id contract-id)
                          fee (:fee contract)
                          premium (:premium contract)]
                      (log/infof "BITCOIN *** Successful payment to %s with transaction %s funds contract ID %s"
                                 address-payed tx-hash contract-id)
                      (db/contract-set-escrow-funded! contract-id amount tx-hash)
                      (db/save-current-wallet (wallet-serialize wallet))
                      (unfollow-transaction! tx-hash)
                      (let [{:keys [escrow-tx escrow-script]}
                            (create-multisig app wallet
                                             (common/currency-discount
                                              (common/currency-discount amount fee 2)
                                              premium
                                              2)
                                             (.getBytes (db/contract-get-field contract-id "escrow_our_key"))
                                             (escrow/get-seller-key contract-id)
                                             (escrow/get-buyer-key contract-id))]
                        (db/contract-update! contract-id {:escrow_tx escrow-tx
                                                          :escrow_script escrow-script}))
                      (db/save-current-wallet (wallet-serialize wallet)))
                    (or (= tx-confidence-val transaction-confidence:dead)
                        (= tx-confidence-val transaction-confidence:in-conflict))
                    (do (log/errorf "ALERT BITCOIN *** TRANSACTION CONFIDENCE TYPE CHANGED TO %s. Transaction was funding contract ID %s" tx-confidence-type contract-id)
                        (unfollow-transaction! tx-hash))
                    ;; UNKNOWN and PENDING don't require action
                    :else nil))))
        (catch Exception e
          (log/debugf "Exception in confidence listener: %s" (with-out-str (pprint e))))))))

(defn wallet-add-listeners! [app wallet]
  (let [recv-li (make-coins-received-listener app)
        trans-li (make-transaction-confidence-listener app wallet)]
    (.addCoinsReceivedEventListener wallet recv-li)
    (.addTransactionConfidenceEventListener wallet trans-li)
    {:coins-received recv-li :transaction-confidence trans-li}))

(defn wallet-remove-listeners! [wallet listeners]
  (.removeCoinsReceivedEventListener wallet (:coins-received listeners))
  (.removeTransactionConfidenceEventListener wallet (:transaction-confidence listeners)))

;;
;; Multisig
;;

;; (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))
;; (. org.bitcoinj.core.Base58 encode (.getSecretBytes (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))))
;; Private key: "GMPBVAgMHJ6jUk7g7zoYhQep5EgpLAFbrJ4BbfxCr9pp"
;; Address: "mjd3Ug6t53rbsrZhZpCdobqHDUnU8sjAYd"

(defn create-multisig [app wallet amount our-key-bytes seller-key-bytes buyer-key-bytes]
  (let [our-key (. ECKey fromPrivate our-key-bytes)
        seller-key (. ECKey fromPrivate seller-key-bytes)
        buyer-key (. ECKey fromPrivate buyer-key-bytes)
        keys (. ImmutableList of our-key seller-key buyer-key)
        script (. ScriptBuilder createMultiSigOutputScript 2 keys)
        tx (Transaction. (:network-params app))
        value (- amount
                 (if (= (env :env) "production")
                   70000 ; http://bitcoinexchangerate.org/fees
                   100000))
        _ (.addOutput tx (. Coin valueOf value) script)
        request (. SendRequest forTx tx)]
    (.completeTx wallet request)
    (.commitTx wallet (.tx request))
    (db/save-current-wallet (wallet-serialize wallet))
    (.broadcastTransaction (:peergroup app) (.tx request))
    {:escrow-tx (.getHashAsString (.tx request))
     :escrow-script (.getProgram script)}))

(defn multisig-spend [app wallet escrow-script input-tx target-address key1 key2]
  (let [input-tx (.getTransaction wallet (. Sha256Hash create (.getBytes input-tx)))
        multisig-out (first (filter #(.isMine %) (.getOutputs input-tx)))
        value (.getValue multisig-out)
        ;; Signature 1
        tx1 (Transaction. (:network-params app))
        _ (.addOutput tx1 value (. Address fromBase58 (:network-params app) target-address))
        _ (.addInput tx1 multisig-out)
        signature1 (.calculateSignature tx1
                                        0
                                        key1
                                        escrow-script
                                        org.bitcoinj.core.Transaction$SigHash/ALL
                                        false)
        ;; Signature 2
        tx2 (Transaction. (:network-params app))
        _ (.addOutput tx2 value (. Address fromBase58 (:network-params app) target-address))
        input (.addInput tx2 multisig-out)
        signature2 (.calculateSignature tx2
                                        0
                                        key2
                                        escrow-script
                                        org.bitcoinj.core.Transaction$SigHash/ALL
                                        false)
        input-script (. ScriptBuilder createMultiSigInputScript [signature1 signature2])]
    (.setScriptSig input input-script)
    (.verify input multisig-out)
    (.commitTx wallet tx2)
    (db/save-current-wallet (wallet-serialize wallet))
    (.broadcastTransaction (:peergroup app) tx2)
    (.getHashAsString tx2)))

;;
;; P2SH Multisig (not supported)
;;

;; Ref: http://www.soroushjp.com/2014/12/20/bitcoin-multisig-the-hard-way-understanding-raw-multisignature-bitcoin-transactions/

(defrecord P2SHMultiSig [our-key seller-key buyer-key p2sh-script redeem-script])

(defn create-p2hs-multisig [app]
  (let [our-key (ECKey.)
        ;;  We can get the other parties public key from somewhere
        ;; ECKey serverKey = new ECKey(null, publicKeyBytes);
        seller-key (ECKey.)
        buyer-key (ECKey.)
        keys (. ImmutableList of our-key seller-key buyer-key)
        redeem-script (. ScriptBuilder createRedeemScript 2 keys)
        p2sh-script (. ScriptBuilder createP2SHOutputScript redeem-script)]
    (P2SHMultiSig. our-key seller-key buyer-key p2sh-script redeem-script)))

(defn p2hs-multisig-address [multisig app]
  (. Address fromP2SHHash (:network-params app) (.getPubKeyHash (:p2sh-script multisig))))

;;
;; Init
;;

(defn system-start! []
  (swap! current-app #(or % (make-app)))
  (swap! current-wallet #(or %
                             (when-let [w (db/load-current-wallet)]
                               (let [wallet (wallet-deserialize w)]
                                 (log/debug "Restoring wallet from database...")
                                 (log/debugf "Wallet balance: %s" (common/satoshi->btc (wallet-get-balance wallet)))
                                 (reset! current-listeners (wallet-add-listeners! @current-app wallet))
                                 wallet))
                             (do (log/warn "Creating new wallet!")
                                 (let [w (make-wallet @current-app)]
                                   (reset! current-listeners (wallet-add-listeners! @current-app w))
                                   (db/save-current-wallet (wallet-serialize w))
                                   w))))
  (app-add-wallet @current-app @current-wallet)
  (app-start! @current-app))

(defn system-stop! []
  (when @current-app
    (wallet-remove-listeners! @current-wallet)
    (.stop (:peergroup @current-app))
    'ok))

(defn reset-listeners! []
  (wallet-remove-listeners! @current-wallet @current-listeners)
  (reset! current-listeners (wallet-add-listeners! @current-app @current-wallet)))

(defn system-reset!!! []
  (system-stop!)
  (reset! current-app nil)
  (reset! current-wallet nil))

;;
;; Notes
;;

;; See SendRequest on how to empty wallets (emptyWallet field, or emptyWallet static method)

(comment
  (def app (make-app))
  (app-start! app)
  (def w (make-wallet app))
  (def app (app-add-wallet app w))
  (def multisig (create-multisig app w (btc->satoshi 1)))
  (multisig-spend multisig
                  app
                  (first (.getTransactionsByTime w))
                  (.toString (wallet-get-current-address w))
                  (:our-key multisig)
                  (:seller-key multisig))
  (def mtx (multisig-setup-from-wallet multisig w app)))


(defn test-wallet []
  (let [addr (wallet-get-current-address @current-wallet)]
    (db/contract-update! 1 {:input_address addr})
    addr))
