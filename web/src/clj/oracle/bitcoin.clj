(ns oracle.bitcoin
  (:require [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]])
  (:import [java.io File]
           [com.google.common.collect ImmutableList]
           [org.bitcoinj.core Address Coin ECKey Transaction]
           [org.bitcoinj.params MainNetParams TestNet3Params]
           [org.bitcoinj.kits WalletAppKit]
           [org.bitcoinj.utils BriefLogFormatter]
           [org.bitcoinj.script ScriptBuilder]
           [org.bitcoinj.wallet SendRequest]))

(defonce network-params (atom nil))
(defonce app-kit (atom nil))

(defn init! []
  (. BriefLogFormatter init)
  (reset! network-params (case (env :env)
                           ("production") (. MainNetParams get)
                           (. TestNet3Params get)))
  (reset! app-kit (WalletAppKit. @network-params (File. ".") (case (env :env)
                                                               ("production") "mainnet"
                                                               "testnet")))
  (.startAsync @app-kit))

(defn wait-for-client []
  (.awaitRunning @app-kit))

(defrecord MultiSigWallet [our-key seller-key buyer-key contract script])

;; (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))
;; (. org.bitcoinj.core.Base58 encode (.getSecretBytes (. ECKey fromPrivate (BigInteger. "103169733757778458218722489788847239787967310021819641785899608061388154372397"))))
;; Private key: "GMPBVAgMHJ6jUk7g7zoYhQep5EgpLAFbrJ4BbfxCr9pp"
;; Address: "mjd3Ug6t53rbsrZhZpCdobqHDUnU8sjAYd"

(defn get-current-wallet []
  (. @app-kit wallet))

(defn get-current-network-params []
  (. @app-kit params))

(defn get-current-peergroup []
  (. @app-kit peerGroup))

(defn eckey-from-address [address]
  (. ECKey fromPublicOnly ()))

(defn create-multisig [network-params]
  (let [our-key (ECKey.)
        ;;our-key (. wallet currentReceiveKey)
        ;;  We can get the other parties public key from somewhere
        ;; ECKey serverKey = new ECKey(null, publicKeyBytes);
        seller-key (ECKey.)
        buyer-key (ECKey.)
        keys (. ImmutableList of our-key seller-key buyer-key)
        ;; Prepare a template for the contract.
        contract (Transaction. network-params)
        script (. ScriptBuilder createMultiSigOutputScript 2 keys)]
    (MultiSigWallet. our-key seller-key buyer-key contract script)))

(defn multisig-get-our-address [multisig network-params]
  (.toString (.toAddress (:our-key multisig) network-params)))

(defn wallet-get-balance [wallet] (.getBalance wallet))

(defn multisig-setup [multisig wallet peergroup]
  (let [wallet-tx (first (.getUnspents wallet))
        coins (get-wallet-balance wallet) ;;(. Coin valueOf 0 0001)
        output (.addOutput (:contract multisig) coins (:script multisig))
        input (.addInput (:contract multisig) wallet-tx)]
    (let [request (. SendRequest forTx (:contract multisig))]
      (try (.completeTx wallet request)
           (. peergroup broadcastTransaction (.tx request))
           (catch Exception e (println e))))))

(defn wallet-send-coins [wallet peergroup target-address amount]
  (.sendCoins wallet
              peergroup
              (. Address fromBase58 (.getNetworkParameters wallet) target-address)
              (. Coin valueOf amount)))

(defn multisig-send-ours [multisig network-params target-address]
  (let [multisig-output (.getOutput (:contract multisig) 0)
        ;;multisig-script (.getScriptPubKey transaction-output)
        value (.getValue multisig-output)]
    ;;(.isSentToMultiSig (:script multisig))
    (let [spend-tx (Transaction. network-params)]
      (.addOutput spend-tx value (. Address fromBase58 network-params target-address))
      (.addInput spend-tx multisig-output)
      (.calculateSignature spend-tx
                           0
                           (:our-key multisig)
                           (:script multisig)
                           (org.bitcoinj.core.Transaction$SigHash/ALL)
                           false))))

(defn multisig-send-other
  [multisig peergroup network-params target-address our-signature other-key]
  (let [multisig-output (.getOutput (:contract multisig) 0)
        value (.getValue multisig-output)
        spend-tx (Transaction. network-params)]
    (.addOutput spend-tx value (. Address fromBase58 network-params target-address))
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
      (.broadcastTransaction peergroup spend-tx)
      [spend-tx spend-script])))

(comment
  (def multisig (create-multisig (get-current-network-params)))
  (def multisig-address (multisig-get-our-address multisig (get-current-network-params)))
  (multisig-setup multisig (get-current-wallet) (get-current-peergroup))
  (wallet-send-coins (get-current-wallet) (get-current-peergroup) multisig-address 10000)
  (multisig-send-other multisig
                       (get-current-peergroup)
                       (get-current-network-params)
                       "n2eMqTT929pb1RDNuqEnxdaLau1rxy3efi"
                       txsig
                       (:seller-key multisig)))




;; Server side

;; // Assume we get the multisig transaction we're trying to spend from
;; // somewhere, like a network connection.
;; ECKey serverKey = ....;
;; Transaction contract = ....;
;; TransactionOutput multisigOutput = contract.getOutput(0);
;; Script multisigScript = multisigOutput.getScriptPubKey();
;; // Is the output what we expect?
;; checkState(multisigScript.isSentToMultiSig());
;; Coin value = multisigOutput.getValue();

;; // OK, now build a transaction that spends the money back to the client.
;; Transaction spendTx = new Transaction(params);
;; spendTx.addOutput(value, clientKey);
;; spendTx.addInput(multisigOutput);

;; // It's of the right form. But the wallet can't sign it. So, we have to
;; // do it ourselves.
;; Sha256Hash sighash = spendTx.hashTransactionForSignature(0, multisigScript, Transaction.SIGHASH_ALL, false);
;; ECKey.ECDSASignature signature = serverKey.sign(sighash);
;; // We have calculated a valid signature, so send it back to the client:
;; sendToClientApp(signature);


;; Second signature


;; TransactionOutput multisigContract = ....;
;; ECKey.ECSDASignature serverSignature = ....;

;; // Client side code.
;; Transaction spendTx = new Transaction(params);
;; spendTx.addOutput(value, clientKey);
;; TransactionInput input = spendTx.addInput(multisigOutput);
;; Sha256Hash sighash = spendTx.hashTransactionForSignature(0, multisigScript, Transaction.SIGHASH_ALL, false);
;; ECKey.ECDSASignature mySignature = clientKey.sign(sighash);

;; // Create the script that spends the multi-sig output.
;; Script inputScript = ScriptBuilder.createMultiSigInputScript(
;;     ImmutableList.of(mySignature, serverSignature), Transaction.SIGHASH_ALL, false);
;; // Add it to the input.
;; input.setScriptSig(inputScript);

;; // We can now check the server provided signature is correct, of course...
;; input.verify(multisigOutput);  // Throws an exception if the script doesn't run.

;; // It's valid! Let's take back the money.
;; peerGroup.broadcastTransaction(spendTx).get();
;; // Wallet now has the money back in it.


