(ns ^:figwheel-always ethereum.core
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [<! >! chan]]
            cljsjs.web3
            [cljs-web3.core :as web3]
            [cljs-web3.eth :as web3-eth]
            [cljs-web3.db :as web3-db]
            [cljs-web3.personal :as web3-personal]
            [cljs-web3.shh :as web3-shh]
            [cljs-web3.net :as web3-net]
            ;; Internal
            [ethereum.utils :refer [is node-slurp]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;;
;; System init
;;

(nodejs/enable-util-print!)
(def -main (fn [] nil))
(set! *main-cli-fn* -main)
(js* "Web3 = require('web3'); web3 = new Web3();")

(def w3 (web3/create-web3 "http://localhost:8545"))

;; (try (is (web3-personal/unlock-account w3 (web3-eth/default-account w3) "hd2tngNgdn56lbAZznxemmlnSnFMcPdogNGDnadGFmhNnasd" 999999))
;;      (catch :default e ; js/Error
;;        (reset! is-rpctest? true)))
(def *is-rpctest* (not (nil? (re-find #".*TestRPC.*" (web3/version-node w3)))))
(if *is-rpctest*
  (println "Using in-memory Ethereum blockchain (TestRPC)")
  (println "Using Ethereum blockchain"))

;;
;; Contract tests
;;

(println "-------ooooooo%%%%%%%%  Running Ethereum RPC calls  %%%%%%%%ooooooo-------")

(def contract-source (node-slurp "src/ethereum/sol/contract.sol"))
(def gas-limit 4500000)

(is (web3/connected? w3))
;;(is (string? (web3/version-api w3)))
;;(is (string? (web3/version-ethereum w3)))
(is (seq (web3-eth/accounts w3)))
(is (= (web3/sha3 "1") "0xc89efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bc6"))
(is (= (web3/to-hex "A") "0x41"))
(is (= (web3/to-ascii "0x41") "A"))
(is (= (web3/from-ascii "A") "0x41"))
(is (= (web3/to-decimal "0xFF") 255))
(is (= (web3/from-decimal 255) "0xff"))
(is (= (web3/from-wei 1000000000000000000 :ether) "1"))
(is (= (web3/to-wei 1 :ether) "1000000000000000000"))
(is (.eq (web3/to-big-number 1) 1))
(is (web3/address? "0x6fce64667819c82a8bcbb78e294d7b444d2e1a29"))
(is (not (web3/address? "0x6fce64667819c82a8bcbb78e294d7b444d2e1a294")))
(is (web3/current-provider w3))

(web3-eth/set-default-account! w3 (first (web3-eth/accounts w3)))
(is (= (web3-eth/default-account w3) (first (web3-eth/accounts w3))))

(is (web3-eth/default-block w3))
(is (web3-eth/syncing? w3))

(is (web3-eth/coinbase w3))
(is (number? (web3-eth/hashrate w3)))

(is (web3-net/listening? w3))
(is (number? (web3-net/peer-count w3)))

(is (number? (.toNumber (web3-eth/gas-price w3))))
(is (number? (.toNumber (web3-eth/get-balance w3 (web3-eth/coinbase w3)))))

(is (map? (web3-eth/get-block w3 "latest")))
(is (seq (web3-eth/get-compilers w3)))


(defn get-contract [compiled]
  ;; Difference testrpc
  (or (:test compiled) compiled))

(let [create-contract-ch (chan)]
  (let [compiled (web3-eth/compile-solidity w3 contract-source)]
    (is (map? compiled))
    (is (number? (web3-eth/estimate-gas w3 (:info (get-contract compiled)))))
    (web3-eth/contract-new w3
                           (:abi-definition (:info (get-contract compiled)))
                           {:data (:code (:test compiled))
                            :gas gas-limit
                            :from (first (web3-eth/accounts w3))}
                           #(go (>! create-contract-ch [%1 %2]))))
  (go
    (let [[err contract] (<! create-contract-ch)]
      (is (not err))
      (is contract)
      (is (not (:address contract)))
      (is (map? (web3-eth/get-transaction w3 (aget contract "transactionHash")))))
    (let [[err contract] (<! create-contract-ch)]
      (is (not err))
      (is (aget contract "address"))
      (is (string? (web3-eth/contract-call contract :multiply 5)))
      (println "-------ooooooo%%%%%%%%  Done  %%%%%%%%ooooooo-------"))))


;; DOCUMENTATION


;; web3.eth.accounts
;; web3.version.api
;; web3.eth.defaultAccount
;; web3.isConnected()
;; web3.net.peerCount
;; web3.net.getPeerCount(function(error, result){ ... })
;; becomes

;; (web3-eth/accounts web3)
;; (web3/version-api web3)
;; (web3-eth/default-account web3)
;; (web3/connected? web3)
;; (web3-net/peer-count web3)
;; (web3-net/peer-count web3 (fn [error result]))
;; Some functions in cljs-web3.core don't really need web3 instance, even though they're called as object methods in Web3 docs. To make our lives easier, in clojurescript, you can just call it without web3 instance. For example:

;; (web3/sha3 "1")
;; => 0xc89efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bc6
;; (web3/to-hex "A")
;; => 0x41
;; (web3/to-wei 1 :ether)
;; => "1000000000000000000"



;; Thses are few extra functions, which you won't find in Web3 API

;; ; Create web3 instance
;; (web3/create-web3 "http://localhost:8545/")

;; ; Deploy new contract
;; (web3-eth/contract-new web3 abi
;;   {:data bin
;;    :gas gas-limit
;;    :from (first (web3-eth/accounts w3))}
;;   (fn [err res]))

;; ; Create contract instance from already deployed contract
;; (web3-eth/contract-at web3 abi address)

;; ; This way you can call any contract method
;; (web3-eth/contract-call ContractInstance :multiply 5)
