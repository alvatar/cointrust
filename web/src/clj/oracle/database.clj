(ns oracle.database
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [clojure.java.jdbc :as sql]
            [crypto.random :as crypto]
            [cheshire.core :as json]
            [camel-snake-kebab.core :as case-shift]
            [taoensso.nippy :as nippy]
            ;; -----
            [oracle.utils :as utils]))

;; CURRENCY-BUYER: the currency on the side of the buyer before the transaction (what the buyer *has*)
;; CURRENCY-SELLER: the currency on the side of the seller before the transaction (what the seller *has*)
;; AMOUNT: in currency-seller units

;; References:
;; http://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql.html

(def db (or (env :database-url)
            "postgresql://localhost:5432/oracledev"))

(defn override-db [new-db]
 (def db (str new-db "?ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory")))

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

(defn ->kebab-case [r] (reduce-kv #(assoc %1 (case-shift/->kebab-case %2) %3) {} r))

;;
;; Events
;;

(defn log! [level type datamap]
  (sql/insert! db :logs {:level level :type type :data (json/generate-string datamap)}))

;; Used only internally to log database operations
(defn- log-op! [tx-or-db type datamap]
  (sql/execute! tx-or-db ["
INSERT INTO logs (level, type, data) VALUES ('debug', ?, ?) RETURNING *;
" type (json/generate-string datamap)]))

(defn get-all-logs [limit]
  (sql/query db ["SELECT * FROM logs LIMIT ?" limit]))

;;
;; Users and friends
;;

(defn user-insert! [user-fbid user-name user-hash friend-hashes]
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
INSERT INTO user_account (fb_id, name, hash) VALUES (?, ?, ?)
ON CONFLICT (hash) DO UPDATE SET hash = ?
RETURNING id;
" user-fbid user-name user-hash user-hash])
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
      (log-op! tx "user-insert" {:id user-id
                                 :name user-name
                                 :hash user-hash
                                 :friend-hashes friend-hashes})
      {:user user-id
       :name user-name
       :friends friend-ids})))

(defn get-users []
  (sql/query db ["SELECT * FROM user_account;"]))

(defn get-friends []
  (sql/query db ["SELECT * FROM friends;"]))

(defn get-user-by-id [id]
  (first (sql/query db ["SELECT * FROM user_account WHERE id = ?;" id])))

(defn get-user-by-hash [hash]
  (first (sql/query db ["SELECT * FROM user_account WHERE hash = ?;" hash])))

(defn get-user-friends [id]
  (sql/query db ["
SELECT user_id2 AS user FROM friends
WHERE user_id1 = ?
UNION
SELECT user_id1 AS user FROM friends
WHERE user_id2 = ?
" id id]))

(defn get-user-edges [id]
  (sql/query db ["
SELECT user_id1, user_id2 FROM user_account, friends
WHERE (user_id1 = ? OR user_id2 = ?) AND id = ?
" id id id]))

(defn get-user-friends-of-friends [id]
  (map
   ->kebab-case
   (sql/query db ["
SELECT id,fb_id FROM user_account
WHERE id IN
(WITH user_friends AS (
          SELECT user_id2 AS u FROM friends
          WHERE user_id1 = ?
          UNION
          SELECT user_id1 AS u FROM friends
          WHERE user_id2 = ?
     )
SELECT user_id1 AS user FROM friends
WHERE (user_id1 IN (SELECT u FROM user_friends) OR user_id2 IN (SELECT u FROM user_friends))
      AND NOT user_id1 = ?
UNION
SELECT user_id2 AS user FROM friends
WHERE (user_id1 IN (SELECT u FROM user_friends) OR user_id2 IN (SELECT u FROM user_friends))
      AND NOT user_id2 = ?)
" id id id id])))

;;
;; Sell Offers
;;

(defn sell-offer-set! [user-id currency minval maxval premium]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
INSERT INTO sell_offer (user_id, currency, min, max, premium) VALUES (?, ?, ?, ?, ?)
ON CONFLICT (user_id) DO UPDATE SET currency = ?, min = ?, max = ?, premium = ?
" user-id currency minval maxval premium currency minval maxval premium])
    (log-op! tx "sell-offer-set"
             {:user-id user-id :currency currency :min minval :max maxval :premium premium})))

(defn sell-offer-unset! [user-id]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
DELETE FROM sell_offer WHERE user_id = ?;
" user-id])
    (log-op! tx "sell-offer-unset" {:user-id user-id})))

(defn get-sell-offer-by-user [user-id]
  (first
   (sql/query db ["
SELECT user_id AS user, currency, min, max, premium FROM sell_offer WHERE user_id = ?;
" user-id])))

(defn get-all-sell-offers []
  (into []
        (sql/query db ["
SELECT * FROM sell_offer;
"])))

;;
;; Buy Requests
;;

(defn buy-request-create! [user-id amount currency-buyer currency-seller exchange-rate & [txcb]]
  (sql/with-db-transaction
    [tx db]
    (let [buy-request (first
                       (sql/query tx ["
INSERT INTO buy_request (buyer_id, amount, currency_buyer, currency_seller, exchange_rate)
VALUES (?, ?, ?, ?, ?)
RETURNING *;
" user-id amount currency-buyer currency-seller exchange-rate]))]
      (log-op! tx "buy-request-create" {:user-id user-id
                                     :amount amount
                                     :currency-buyer currency-buyer
                                     :currency-seller currency-seller
                                     :exchange-rate exchange-rate})
      (let [kb-buy-request (->kebab-case buy-request)]
        (when txcb (txcb kb-buy-request))
        kb-buy-request))))

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

(defn buy-request-set-field! [id field value]
  (sql/execute! db [(format "
UPDATE buy_request SET %s = ?
WHERE id = ?
" field) value id]))

(defn buy-request-set-seller! [buy-request seller-id & [txcb]]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
UPDATE buy_request SET seller_id = ?
WHERE id = ?
" seller-id buy-request])
    (log-op! tx "buy-request-set-seller" {:id buy-request :seller-id seller-id})
    (when txcb (txcb seller-id)))
  seller-id)

(defn buy-request-unset-seller! [buy-request]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
UPDATE buy_request SET seller_id = NULL
WHERE id = ?
" buy-request])
    (log-op! tx "buy-request-unset-seller" {:id buy-request})))

(defn buy-request-delete! [buy-request]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
DELETE FROM buy_request WHERE id = ?;
" buy-request])
    (log-op! tx "buy-request-delete" {:id buy-request}))
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
  [{:keys [buyer-id seller-id amount currency-buyer currency-seller exchange-rate fee premium transfer-info input-address] :as params} & [txcb]]
  (try
    (when-not (= buyer-id seller-id)
      (let [buyer-fbid (:fb_id (get-user-by-id buyer-id))
            seller-fbid (:fb_id (get-user-by-id seller-id))]
        (sql/with-db-transaction
          [tx db]
          (loop [human-id (utils/human-id-generator)]
            (if (not-empty (first (sql/query db ["SELECT id FROM contract WHERE human_id = ?" human-id])))
              (do (log/errorf "Found collision in human-id generator: %s" human-id) (recur (utils/human-id-generator)))
              (let [init-stage "waiting-escrow" contract (first (sql/query tx ["
INSERT INTO contract (human_id, hash, buyer_id, buyer_fbid, seller_id, seller_fbid, amount, fee, premium, currency_buyer, currency_seller, exchange_rate, transfer_info, input_address, escrow_address, escrow_our_key)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
RETURNING *;
" human-id (random-string 27) buyer-id buyer-fbid seller-id seller-fbid amount fee premium currency-buyer currency-seller exchange-rate transfer-info input-address "<escrow-address>" "<escrow-our-key>"]))]
                (sql/execute! tx ["
INSERT INTO contract_event (contract_id, stage) VALUES (?, ?);
" (:id contract) init-stage])
                (let [contract (merge contract {:stage init-stage})]
                  (log-op! tx "contract-create" params)
                  (when txcb (txcb contract))
                  contract)))))))
    (catch Exception e
      (log/debugf "Error in contract creation with params: %s" (with-out-str (pprint params)))
      (throw e))))

(defn contract-add-event! [contract-id stage & [data txcb]]
  (sql/with-db-transaction
    [tx db]
    (sql/execute! tx ["
INSERT INTO contract_event (contract_id, stage, data) VALUES (?, ?, ?);
" contract-id stage (when data (nippy/freeze data) "")])
    (log-op! tx "contract-add-event" {:id contract-id :stage stage :data (or data "")})
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
WHERE contract_event.time = ( SELECT MAX(contract_event.time) FROM contract_event
                              WHERE contract_event.contract_id = ? );
" contract-id])))

(defn get-contracts-by-user [user-id]
  (map ->kebab-case
       (sql/query db ["
SELECT * FROM contract
WHERE buyer_id = ? OR seller_id = ?
ORDER BY contract.created DESC
" user-id user-id])))

(defn get-contracts-by-user-with-last-event [user-id]
  (map ->kebab-case
       (sql/query db ["
SELECT * FROM contract
INNER JOIN (
  SELECT a.*
  FROM contract_event a
  INNER JOIN (
      SELECT contract_id, MAX(time) AS max_time
      FROM contract_event
      GROUP BY contract_id
  ) b ON a.contract_id = b.contract_id AND a.time = b.max_time
) latest_events
ON contract.id = latest_events.contract_id
WHERE (buyer_id = ? OR seller_id = ?)
ORDER BY contract.created DESC
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
JOIN contract
ON contract_event.contract_id = contract.id
WHERE contract.id = ?
ORDER BY contract_event.time DESC
LIMIT 1
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
  (map ->kebab-case
       (sql/query db ["
SELECT * FROM contract
"])))

(defn get-all-events []
  (mapv
   ->kebab-case
   (sql/query db ["
SELECT * FROM contract_event;
"])))

(defn get-all-last-events []
  (mapv ->kebab-case
        (sql/query db ["
SELECT a.*
FROM contract_event a
INNER JOIN (
    SELECT contract_id, MAX(time) AS max_time
    FROM contract_event
    GROUP BY contract_id
) b ON a.contract_id = b.contract_id AND a.time = b.max_time
" ])))

;; TODO: IS THIS SECURE?

;; TODO: instead of setting the timestamp here, use the timestamp from latest event

(defn contract-set-escrow-funded! [id amount-received tx-hash]
  (sql/execute! db ["
UPDATE contract SET escrow_funded = true, escrow_amount = ?, input_tx = ?, escrow_funded_timestamp = CURRENT_TIMESTAMP
WHERE id = ?
" amount-received tx-hash id]))

(defn contract-set-field! [id field value]
  (sql/execute! db [(format "
UPDATE contract SET %s = ?
WHERE id = ?
" field) value id]))

;;
;; Wallet
;;

(defn load-current-wallet []
  (->
   (sql/query db ["
SELECT data FROM wallet;
"])
   first
   :data))

(defn save-current-wallet [w]
  (sql/execute! db ["
INSERT INTO wallet (data) VALUES (?)
ON CONFLICT (lock) DO UPDATE SET data = ?;
" w w]))

;;
;; Development utilities
;;

(defn reset-database!!! []
  (sql/db-do-commands db
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
  hash                             VARCHAR(128) NOT NULL UNIQUE,
  fb_id                            BIGINT NOT NULL,
  name                             VARCHAR(256)
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
  currency                         VARCHAR(8) NOT NULL,
  min                              BIGINT NOT NULL,
  max                              BIGINT NOT NULL,
  premium                          INT NOT NULL
);"
                       "
CREATE TABLE buy_request (
  id                               SERIAL PRIMARY KEY,
  created                          TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  buyer_id                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  seller_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE,
  amount                           BIGINT NOT NULL,
  premium                          INT,
  currency_buyer                   VARCHAR(8) NOT NULL,
  currency_seller                  VARCHAR(8) NOT NULL,
  exchange_rate                    DECIMAL(26,6) NOT NULL
);"
                       "
CREATE TABLE contract (
  id                               SERIAL PRIMARY KEY,
  hash                             VARCHAR(128) NOT NULL UNIQUE,
  human_id                         VARCHAR(256) NOT NULL UNIQUE,
  created                          TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  buyer_id                         INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  buyer_fbid                       BIGINT,
  seller_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  seller_fbid                      BIGINT,
  amount                           BIGINT NOT NULL,
  currency_buyer                   VARCHAR(8) NOT NULL,
  currency_seller                  VARCHAR(8) NOT NULL,
  exchange_rate                    DECIMAL(26,6) NOT NULL,
  fee                              INT NOT NULL,
  premium                          INT NOT NULL,
  input_address                    VARCHAR(128),
  input_tx                         VARCHAR(128),
  escrow_address                   VARCHAR(128),
  output_address                   VARCHAR(128),
  output_tx                        VARCHAR(128),
  escrow_our_key                   VARCHAR(128),
  escrow_buyer_has_key             BOOLEAN DEFAULT false,
  escrow_seller_has_key            BOOLEAN DEFAULT false,
  escrow_funded                    BOOLEAN DEFAULT false,
  escrow_funded_timestamp          TIMESTAMP,
  escrow_amount                    BIGINT,
  escrow_open_for                  INTEGER REFERENCES user_account(id) ON UPDATE CASCADE ON DELETE CASCADE,
  escrow_release                   VARCHAR(64) DEFAULT '<fresh>',
  transfer_info                    TEXT NOT NULL,
  transfer_sent                    BOOLEAN DEFAULT false,
  transfer_received                BOOLEAN DEFAULT false
);"
                       "
CREATE TABLE contract_event (
  id                               SERIAL PRIMARY KEY,
  time                             TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  contract_id                      INTEGER REFERENCES contract(id) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  stage                            VARCHAR(64) NOT NULL,
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
  level                            VARCHAR(8) NOT NULL,
  type                             VARCHAR(64) NOT NULL,
  data                             TEXT NOT NULL
);"
                       ]))
;; Only one entry lock:
;; lock                      CHAR(1) NOT NULL DEFAULT('X'),
;; constraint only_one       PRIMARY KEY (lock),
;; constraint CK_T1_locked   CHECK (lock='X'),
