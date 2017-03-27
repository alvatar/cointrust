(ns oracle.bitcoin
  (:import [java.io File ByteArrayOutputStream ByteArrayInputStream]
           [java.net InetAddress]
           [java.util ArrayList]
           [com.google.common.collect ImmutableList]
           [org.bitcoinj.core Address BlockChain Coin Context ECKey PeerAddress PeerGroup Transaction
            TransactionBroadcast]
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
            [cemerick.url :refer [url]]
            [cheshire.core :as json]
            [taoensso.carmine :as r]
            ;; -----
            [oracle.common :as common]
            [oracle.redis :as redis]
            [oracle.database :as db]))

;;
;; Utils
;;

(def global-fee 5000)

(defn substract-satoshi-fee [sat]
  (- sat global-fee))

(defn substract-btc-fee [sat]
  (- sat (common/satoshi->btc global-fee)))

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

;;
;; App
;;

(defrecord App [network-params blockchain peergroup wallets])

(defn make-app []
  (let [network-params (case (env :env)
                         "production" (. MainNetParams get)
                         "staging" (. TestNet3Params get)
                         (. RegTestParams get))
        uri (url (str "http" (subs (or (env :database-url) "postgres://alvatar:@localhost:5432/oracledev") 8)))
        blockchain (BlockChain. network-params
                                ;;(MemoryBlockStore. network-params)
                                (SPVBlockStore. network-params (File. ".spvchain"))
                                ;; According to the documentation 1000 blocks stored is safe
                                #_(PostgresFullPrunedBlockStore.
                                   network-params 1000
                                   (format "%s:%s" (:host uri) (:port uri))
                                   (subs (:path uri) 1)
                                   (:username uri)
                                   (:password uri)))]
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
                                 (.stop (:peergroup app))
                                 ;; TODO: Save wallets to their Files
                                 (.close (.getBlockStore (:blockchain app))))))
    listener))

(defn app-stop! [app]
  (.shutDown app))

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

(defn get-current-wallet [] @current-wallet)

(defn wallet-get-current-address [wallet]
  (.toString (.currentReceiveAddress wallet)))

(defn wallet-get-fresh-address [wallet]
  (.toString (.freshReceiveAddress wallet)))

(defn wallet-get-balance [wallet]
  (.getValue (.getBalance wallet)))

(defn wallet-send-coins [wallet app contract-id target-address amount]
  (try (let [send-result (.sendCoins wallet
                                     (:peergroup app)
                                     (. Address fromBase58 (.getNetworkParameters wallet) target-address)
                                     (. Coin valueOf amount))
             tx (.-tx send-result)
             tx-hash (.. tx getHash toString)]
         (db/contract-update! contract-id {:output_tx tx-hash})
         (btc-log! "Send payment for contract %s of %s BTC to address %s transaction %s\n"
                   contract-id (common/satoshi->btc amount) target-address tx-hash))
       (catch Exception e
         (log/debugf "Error sending coins: %s" e) nil)))

(defn wallet-send-all-funds-to [wallet app target-address]
  (wallet-send-coins wallet app target-address
                     (substract-satoshi-fee (wallet-get-balance wallet))))

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

(def coins-received-listener
  (reify org.bitcoinj.wallet.listeners.WalletCoinsReceivedEventListener
    (onCoinsReceived [this wallet transaction prev-balance new-balance]
      (try (btc-log! "Wallet balance: %s" (common/satoshi->btc (wallet-get-balance wallet)))
           (let [amount-payed (- (.getValue new-balance) (.getValue prev-balance))]
             (doseq [output (.getOutputs transaction)]
               (when-let [address-p2pk (.getAddressFromP2PKHScript output (:network-params @current-app))]
                 (let [tx-hash (.. transaction getHash toString)
                       address-payed (.toString address-p2pk)]
                   (if (pos? amount-payed)
                     (if-let [contract (db/get-contract-by-input-address address-payed)]
                       (do (log/infof "BITCOIN *** Following payment to %s funds contract ID %s" address-payed (:id contract))
                           (follow-transaction! (.getHashAsString transaction) address-payed amount-payed (:id contract)))
                       (log/errorf "BITCOIN *** CRITICAL: payment of %s BTC in address %s is not associated to any contract\n"
                                   (common/satoshi->btc amount-payed) address-payed))
                     (btc-log! "Sent payment of %s BTC to address %s\n" (- (common/satoshi->btc amount-payed)) address-payed))))))
           (catch Exception e (log/error e))))))

(def transaction-confidence-listener
  (reify org.bitcoinj.core.listeners.TransactionConfidenceEventListener
    (onTransactionConfidenceChanged [this wallet transaction]
      (let [tx-hash (.getHashAsString transaction)]
        (when-let [tx-data (followed-transaction tx-hash)]
          (let [confidence (.getConfidence transaction)
                tx-depth (.getDepthInBlocks confidence)
                tx-confidence-type (.getConfidenceType confidence)
                address-payed (:address tx-data)
                contract-id (:contract-id tx-data)]
            (cond (>= tx-depth 1)
                  (do (log/infof "BITCOIN *** Payment to %s funds contract ID %s" address-payed contract-id)
                      (db/contract-set-escrow-funded! contract-id (:amount tx-data) tx-hash)
                      (db/save-current-wallet (wallet-serialize wallet))
                      (unfollow-transaction! tx-hash))
                  (zero? tx-depth)
                  (log/errorf "ALERT BITCOIN *** TRANSACTION CONFIDENCE TYPE CHANGED TO %s. Transaction was funded contract ID %s" tx-confidence-type contract-id))))))))

(defn wallet-add-listeners! [wallet]
  (.addCoinsReceivedEventListener wallet coins-received-listener)
  (.addTransactionConfidenceEventListener wallet transaction-confidence-listener))

(defn wallet-remove-listeners! [wallet]
  (.removeCoinsReceivedEventListener wallet coins-received-listener)
  (.removeTransactionConfidenceEventListener wallet transaction-confidence-listener))

;;
;; Multisig
;;

;; (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))
;; (. org.bitcoinj.core.Base58 encode (.getSecretBytes (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))))
;; Private key: "GMPBVAgMHJ6jUk7g7zoYhQep5EgpLAFbrJ4BbfxCr9pp"
;; Address: "mjd3Ug6t53rbsrZhZpCdobqHDUnU8sjAYd"

(defrecord MultiSig [our-key seller-key buyer-key tx script])

(defn create-multisig [app wallet value]
  (let [our-key (ECKey.)
        seller-key (ECKey.)
        buyer-key (ECKey.)
        keys (. ImmutableList of our-key seller-key buyer-key)
        script (. ScriptBuilder createMultiSigOutputScript 2 keys)
        tx (Transaction. (:network-params app))
        multisig (MultiSig. our-key seller-key buyer-key tx script)
        _ (.addOutput tx (. Coin valueOf value) script)
        request (. SendRequest forTx tx)]
    (.completeTx wallet request)
    (.broadcastTransaction (:peergroup app) (.tx request))
    multisig))

(defn multisig-spend [multisig app input-tx target-address key1 key2 & [key3]]
  (let [multisig-out (.getOutput input-tx 0)
        value (.getValue multisig-out)
        ;; Signature 1
        tx1 (Transaction. (:network-params app))
        _ (.addOutput tx1 value (. Address fromBase58 (:network-params app) target-address))
        _ (.addInput tx1 multisig-out)
        signature1 (.calculateSignature tx1
                                        0
                                        key1
                                        (:script multisig)
                                        org.bitcoinj.core.Transaction$SigHash/ALL
                                        false)
        ;; Signature 2
        tx2 (Transaction. (:network-params app))
        _ (.addOutput tx2 value (. Address fromBase58 (:network-params app) target-address))
        input (.addInput tx2 multisig-out)
        signature2 (.calculateSignature tx2
                                        0
                                        key2
                                        (:script multisig)
                                        org.bitcoinj.core.Transaction$SigHash/ALL
                                        false)
        input-script (. ScriptBuilder createMultiSigInputScript [signature1 signature2])]
    (.setScriptSig input input-script)
    (.verify input multisig-out)
    (.broadcastTransaction (:peergroup app) tx2)))

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
                                 (wallet-add-listeners! wallet)))
                             (do (log/warn "Creating new wallet!")
                                 (let [w (make-wallet @current-app)]
                                   (wallet-add-listeners! w)
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
  (wallet-remove-listeners! @current-wallet)
  (wallet-add-listeners! @current-wallet))

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
