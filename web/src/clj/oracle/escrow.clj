(ns oracle.escrow
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [taoensso.carmine :as r]
            ;; -----
            [oracle.redis :refer :all]
            [oracle.database :as db]
            [oracle.bitcoin :as bitcoin])
  (:import java.util.Base64))


;;
;; Utils
;;

(defn encode-key [to-encode]
  (let [encoded (.encodeToString (Base64/getEncoder) to-encode)]
    (if-let [eqpos (clojure.string/index-of encoded \=)]
      (subs encoded 0 eqpos)
      encoded)))

(defn decode-key [to-decode]
  (.decode (Base64/getDecoder) to-decode))

;;
;; Escrow keys
;;

(defn set-buyer-key [contract-id bkey]
  (wcar* (r/set (str "contract:escrow:buyer-key:" contract-id) (bkey))))

(defn get-buyer-key [contract-id]
  (wcar* (r/get (str "contract:escrow:buyer-key:" contract-id))))

(defn forget-buyer-key [contract-id]
  (= 1 (wcar* (r/del (str "contract:escrow:buyer-key:" contract-id)))))

(defn set-seller-key [contract-id skey]
  (wcar* (r/set (str "contract:escrow:seller-key:" contract-id) skey)))

(defn get-seller-key [contract-id]
  (wcar* (r/get (str "contract:escrow:seller-key:" contract-id))))

(defn forget-seller-key [contract-id]
  (= 1 (wcar* (r/del (str "contract:escrow:seller-key:" contract-id)))))

(defn setup-keys-for-contract! [contract-id]
  (let [keys {:our-key (bitcoin/make-private-key)
              :buyer-key (bitcoin/make-private-key)
              :seller-key (bitcoin/make-private-key)}]
    (db/contract-set-field! contract-id "escrow_our_key" (:our-key keys))
    (set-buyer-key contract-id (:buyer-key keys))
    (set-seller-key contract-id (:seller-key keys))))
