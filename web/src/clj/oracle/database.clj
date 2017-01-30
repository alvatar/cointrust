(ns oracle.database
  (:require [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [clojure.java.jdbc :as sql]
            [crypto.random :as crypto]
            [cheshire.core :as json]
            [camel-snake-kebab.core :as case-shift]))

;; CURRENCY-BUYER: the currency on the side of the buyer before the transaction (what the buyer *has*)
;; CURRENCY-SELLER: the currency on the side of the seller before the transaction (what the seller *has*)
;; AMOUNT: in currency-seller units

;; References:
;; http://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql.html

(def db (or (env :database-url)
            "postgresql://localhost:5432/oracledev"))

(log/debugf "Connecting to PostgreSQL: %s" db)


;;
;; Utils
;;

(def allowed-chars "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def num-allowed-chars (count allowed-chars))

(defn- random-string [n]
  ;; selection is slightly biased towards the first items in `allowed-chars`,
  ;; since 256 is not a multiple of (count allowed-chars). I don't expect this
  ;; to make codes significantly more guessable in practice.
  (apply str (for [b (crypto/bytes n)]
               (nth allowed-chars (mod b num-allowed-chars)))))

;;
;; Events
;;

(defn log! [tx type datamap]
  (sql/execute! tx ["
INSERT INTO logs (type, data) VALUES (?, ?) RETURNING *;
" type (json/generate-string datamap)]))

(defn logs-get-all [limit]
  (sql/query db ["SELECT * FROM logs LIMIT ?" limit]))

;;
;; Users and friends
;;

(defn user-insert! [user-hash friend-hashes]
  ;; TODO: We are currently storing ID too. Just keep hash
  (sql/with-db-transaction
    [tx db]
    (let [friend-ids
          (mapv :id
                (first
                 (for [f friend-hashes]
                   (sql/query tx ["
SELECT id FROM user_account WHERE hash = ?;
" f]))))
          user-id
          (-> (sql/query tx ["
INSERT INTO user_account (hash) VALUES (?)
ON CONFLICT (hash) DO UPDATE SET hash = ?
RETURNING id;
" user-hash user-hash])
              first
              :id)
          friends-insert
          ;; TODO: This doesn't remove old friend hashes. What to do about it?
          (mapv (fn [fid] ; order guarantess uniqueness of edges
                  (let [[user-id1 user-id2] (sort [fid user-id])]
                    (first
                     (sql/execute! tx ["
INSERT INTO friends (user_id1, user_id2)
VALUES (?, ?)
ON CONFLICT DO NOTHING
" user-id1 user-id2]))))
                friend-ids)]
      (log! tx "user-insert" {:user-hash user-hash
                              :friend-hashes friend-hashes})
      {:user user-id
       :friends friend-ids})))

(defn get-users []
  (sql/query db ["SELECT * FROM user_account;"]))

(defn get-friends []
  (sql/query db ["SELECT * FROM friends;"]))

(defn get-user-by-id [id]
  (-> (sql/query db ["SELECT hash FROM user_account WHERE id = ?;" id])
      first
      :hash))

(defn get-user-by-hash [hash]
  (-> (sql/query db ["SELECT id FROM user_account WHERE hash = ?;" hash])
      first
      :id))

(defn get-user-friends [id]
  (sql/query db ["
SELECT user_id2 AS user FROM user_account, friends
WHERE user_id1 = ? AND id = ?
UNION
SELECT user_id1 AS user FROM user_account, friends
WHERE user_id2 = ? AND id = ?
" id id id id]))

(defn get-user-edges [id]
  (sql/query db ["
SELECT user_id1, user_id2 FROM user_account, friends
WHERE (user_id1 = ? OR user_id2 = ?) AND id = ?
" id id id]))

(defn get-user-friends-of-friends [id]
  (mapv :user
        (sql/query db ["
WITH user_friends AS (
          SELECT user_id2 AS user FROM user_account, friends
          WHERE user_id1 = ? AND id = ?
          UNION
          SELECT user_id1 AS user FROM user_account, friends
          WHERE user_id2 = ? AND id = ?
     )
SELECT user_id1 AS user FROM user_account, friends
WHERE NOT (user_id1 = ?) AND id IN (SELECT * FROM user_friends)
UNION
SELECT user_id2 AS user FROM user_account, friends
WHERE NOT (user_id2 = ?) AND id IN (SELECT * FROM user_friends)
" id id id id id id])))

;;
;; Sell Offers
;;

(defn sell-offer-set! [user-id currency minval maxval]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
INSERT INTO sell_offer (user_id, currency, min, max) VALUES (?, ?, ?, ?)
ON CONFLICT (user_id) DO UPDATE SET currency = ?, min = ?, max = ?
" user-id currency minval maxval currency minval maxval])
    (log! tx "sell-offer-set" {:user-id user-id :min minval :max maxval})))

(defn sell-offer-get-by-user [user-id]
  (first
   (sql/query db ["
SELECT min, max FROM sell_offer WHERE user_id = ?;
" user-id])))

(defn sell-offer-unset! [user-id]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
DELETE FROM sell_offer WHERE user_id = ?;
" user-id])
    (log! tx "sell-offer-unset" {:user-id user-id})))

(defn get-all-sell-offers []
  (into []
        (sql/query db ["
SELECT * FROM sell_offer;
"])))

;;
;; Buy Requests
;;

(defn ->kebab-case [r] (reduce-kv #(assoc %1 (case-shift/->kebab-case %2) %3) {} r))

(defn buy-request-create! [user-id amount currency-buyer currency-seller exchange-rate & [txcb]]
  (-> (sql/with-db-transaction
        [tx db]
        (let [buy-request (first
                           (sql/query tx ["
INSERT INTO buy_request (buyer_id, amount, currency_buyer, currency_seller, exchange_rate)
VALUES (?, ?, ?, ?, ?)
RETURNING *;
" user-id amount currency-buyer currency-seller exchange-rate]))]
          (log! tx "buy-request-create" {:user-id user-id
                                         :amount amount
                                         :currency-buyer currency-buyer
                                         :currency-seller currency-seller
                                         :exchange-rate exchange-rate})
          (when txcb (txcb buy-request))
          buy-request))
      ->kebab-case))

(defn get-buy-requests-by-user [user-id]
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM buy_request WHERE buyer_id = ?;
" user-id])))

(defn get-buy-requests-by-counterparty [counterparty]
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM buy_request WHERE seller_id = ?;
" counterparty])))

(defn get-buy-request-by-id [buy-request]
  (-> (sql/query db ["
SELECT * FROM buy_request WHERE id = ?;
" buy-request])
      first
      ->kebab-case))

(defn buy-request-set-seller! [buy-request seller-id & [txcb]]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
UPDATE buy_request SET seller_id = ?
WHERE id = ?
" seller-id buy-request])
    (log! tx "buy-request-set-seller" {:id buy-request :seller-id seller-id})
    (when txcb (txcb seller-id)))
  seller-id)

(defn buy-request-unset-seller! [buy-request]
  (sql/execute! db ["
UPDATE buy_request SET seller_id = NULL
WHERE id = ?
" buy-request]))

(defn buy-request-delete! [buy-request]
  (sql/execute! db ["
DELETE FROM buy_request WHERE id = ?;
" buy-request])
  'ok)

(defn get-all-buy-requests []
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM buy_request;
"])))

;;
;; Contracts
;;

(defn contract-create!
  [{:keys [buyer-id seller-id amount currency-buyer currency-seller exchange-rate transfer-info input-address] :as params} & [txcb]]
  (when-not (= buyer-id seller-id) ;; TODO: check if they are friends^2
    (sql/with-db-transaction
      [tx db]
      (let [init-stage "waiting-escrow" contract (first (sql/query tx ["
INSERT INTO contract (hash, buyer_id, seller_id, amount, currency_buyer, currency_seller, exchange_rate, transfer_info, input_address, escrow_address, escrow_our_key)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
RETURNING *;
" (random-string 27) buyer-id seller-id amount currency-buyer currency-seller exchange-rate transfer-info input-address "<escrow-address>" "<escrow-our-key>"]))]
        (sql/execute! tx ["
INSERT INTO contract_event (contract_id, stage) VALUES (?, ?);
" (:id contract) init-stage])
        (let [contract (merge contract {:stage init-stage})]
          (log! tx "contract-create" params)
          (when txcb (txcb contract))
          contract)))))

(defn contract-add-event! [contract-id stage & [data txcb]]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
INSERT INTO contract_event (contract_id, stage, data) VALUES (?, ?, ?);
" contract-id stage (or data "")])
    (log! tx "contract-add-event" {:id contract-id :stage stage :data (or data "")})
    (when txcb (txcb nil))))

(defn get-contract-events [contract-id]
  (into []
        (sql/query db ["
SELECT contract_event.* FROM contract_event
INNER JOIN contract
ON contract.id = contract_event.contract_id
WHERE contract.id = ?;
" contract-id])))

(defn get-contract-last-event [contract-id]
  (first
   (sql/query db ["
SELECT contract_event.* FROM contract_event
INNER JOIN contract
ON contract.id = contract_event.contract_id
WHERE contract.id = ? AND contract_event.time = (SELECT MAX(contract_event.time) FROM contract_event);
" contract-id])))

(defn get-contracts-by-user [user-id]
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM contract
WHERE buyer_id = ? OR seller_id = ?
" user-id user-id])))

(defn get-contracts-by-user-with-last-event [user-id]
  (mapv ->kebab-case
        (sql/query db ["
SELECT contract.*, contract_event.stage FROM contract_event
INNER JOIN contract
ON contract.id = contract_event.contract_id
WHERE (buyer_id = ? OR seller_id = ?)
      AND contract_event.time = (SELECT MAX(contract_event.time) FROM contract_event);
" user-id user-id])))

(defn get-contract-by-id [id]
  (-> (sql/query db ["
SELECT * FROM contract
WHERE id = ?
" id])
      first
      ->kebab-case))

(defn get-contract-by-id-with-last-event [id]
  (-> (sql/query db ["
SELECT contract.*, contract_event.stage FROM contract_event
INNER JOIN contract
ON contract.id = contract_event.contract_id
WHERE contract.id = ? AND contract_event.time = (SELECT MAX(contract_event.time) FROM contract_event);
" id])
      first
      ->kebab-case))

(defn get-contract-by-input-address [input-address]
  (-> (sql/query db ["
SELECT * FROM contract
WHERE input_address = ?
" input-address])
      first
      ->kebab-case))

(defn get-all-contracts []
  (into []
        (sql/query db ["
SELECT * FROM contract
"])))

(defn get-all-events []
  (sql/query db ["
SELECT * FROM contract_event;
"]))

;; TODO: IS THIS SECURE?

;; TODO: instead of setting the timestamp here, use the timestamp from latest event

(defn contract-set-escrow-funded! [id]
  (sql/execute! db ["
UPDATE contract SET escrow_funded = true, waiting_transfer_start = CURRENT_TIMESTAMP, holding_period_start = CURRENT_TIMESTAMP
WHERE id = ?
" id]))

(defn contract-set-escrow-open-for! [id user-id]
  (sql/execute! db ["
UPDATE contract SET escrow_open_for = ?
WHERE id = ?
" user-id id]))

(defn contract-set-transfer-sent! [id]
  (sql/execute! db ["
UPDATE contract SET transfer_sent = true
WHERE id = ?
" id]))

(defn contract-set-transfer-received! [id]
  (sql/execute! db ["
UPDATE contract SET transfer_received = true, waiting_transfer_start = CURRENT_TIMESTAMP
WHERE id = ?
" id]))

(defn contract-set-output-address! [id output-address]
  (sql/execute! db ["
UPDATE contract SET output_address = ?
WHERE id = ?
" output-address id]))

(defn contract-set-buyer-has-key! [id]
  (sql/execute! db ["
UPDATE contract SET escrow_buyer_has_key = true
WHERE id = ?
" id]))

(defn contract-set-seller-has-key! [id]
  (sql/execute! db ["
UPDATE contract SET escrow_buyer_has_key = true
WHERE id = ?
" id]))

(defn contract-set-field! [id field value]
  (sql/execute! db [(format "
UPDATE contract SET %s = ?
WHERE id = ?
" field) value id]))

;;
;; Wallet
;;

(defn get-current-wallet []
  (->
   (sql/query db ["
SELECT data FROM wallet;
"])
   first
   :data))

(defn set-current-wallet [w]
  (sql/execute! db ["
INSERT INTO wallet (data) VALUES (?)
ON CONFLICT (lock) DO UPDATE SET data = ?;
" w w]))

;;
;; Development utilities
;;

(defn reset-database!!! [& [external-db]]
  (sql/db-do-commands (or (str external-db "?ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory")
                          db)
                      ["DROP TABLE IF EXISTS logs;"
                       "DROP TABLE IF EXISTS wallet;"
                       "DROP TABLE IF EXISTS contract_event;"
                       "DROP TABLE IF EXISTS contract CASCADE;"
                       "DROP TABLE IF EXISTS sell_offer;"
                       "DROP TABLE IF EXISTS buy_request;"
                       "DROP TABLE IF EXISTS friends;"
                       "DROP TABLE IF EXISTS user_account CASCADE;"
                       "
CREATE TABLE user_account (
  id                               SERIAL PRIMARY KEY,
  created                          TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  hash                             TEXT NOT NULL UNIQUE
);"
                       "
CREATE TABLE friends (
  user_id1                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  user_id2                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  PRIMARY KEY (user_id1, user_id2)
);"
                       "
CREATE TABLE sell_offer (
  user_id                          INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  CONSTRAINT one_offer_per_user    UNIQUE (user_id),
  min                              BIGINT NOT NULL,
  max                              BIGINT NOT NULL,
  currency                         TEXT NOT NULL
);"
                       "
CREATE TABLE buy_request (
  id                               SERIAL PRIMARY KEY,
  created                          TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  buyer_id                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  seller_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE,
  amount                           BIGINT NOT NULL,
  currency_buyer                   TEXT NOT NULL,
  currency_seller                  TEXT NOT NULL,
  exchange_rate                    DECIMAL(26,6) NOT NULL
);"
                       "
CREATE TABLE contract (
  id                               SERIAL PRIMARY KEY,
  hash                             TEXT NOT NULL UNIQUE,
  created                          TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  buyer_id                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  seller_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  amount                           BIGINT NOT NULL,
  currency_buyer                   TEXT NOT NULL,
  currency_seller                  TEXT NOT NULL,
  exchange_rate                    DECIMAL(26,6) NOT NULL,
  input_address                    TEXT,
  escrow_address                   TEXT,
  output_address                   TEXT,
  escrow_our_key                   TEXT,
  escrow_buyer_has_key             BOOLEAN,
  escrow_seller_has_key            BOOLEAN,
  escrow_funded                    BOOLEAN,
  escrow_open_for                  INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE,
  transfer_info                    TEXT NOT NULL,
  transfer_sent                    BOOLEAN,
  transfer_received                BOOLEAN,
  waiting_transfer_start           TIMESTAMP,
  holding_period_start             TIMESTAMP
);"
                       "
CREATE TABLE contract_event (
  id                               SERIAL PRIMARY KEY,
  time                             TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  contract_id                      INTEGER REFERENCES contract(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  stage                            TEXT NOT NULL,
  data                             TEXT
);"
                       "
CREATE TABLE wallet (
  lock                             CHAR(1) NOT NULL DEFAULT('X'),
  constraint only_one              PRIMARY KEY (lock),
  constraint CK_T1_locked          CHECK (lock='X'),
  data                             BYTEA
)
"
                       "
CREATE TABLE logs (
  id                               SERIAL PRIMARY KEY,
  time                             TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  type                             TEXT NOT NULL,
  data                             TEXT NOT NULL
);"
                       ]))
;; Only one entry lock:
;; lock                      CHAR(1) NOT NULL DEFAULT('X'),
;; constraint only_one       PRIMARY KEY (lock),
;; constraint CK_T1_locked   CHECK (lock='X'),
