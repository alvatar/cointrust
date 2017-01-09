(ns oracle.database
  (:require [environ.core :refer [env]]
            [taoensso.timbre :as log]
            [clojure.java.jdbc :as sql]
            [crypto.random :as crypto]
            [clojure.data.json :as json]
            [camel-snake-kebab.core :as case-shift]))

;; References:
;; http://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql.html

(def db (or (env :database-url)
            "postgresql://localhost:5432/oracledev"))

(println "Connecting to PostgreSQL:" db)


;;
;; Events
;;

(defn log! [tx type datamap]
  (sql/execute! tx ["
INSERT INTO logs (type, data) VALUES (?, ?) RETURNING *;
" type (json/write-str datamap)]))

(defn logs-get-all [limit]
  (sql/query db ["SELECT * FROM events LIMIT ?" limit]))

;;
;; Users and friends
;;

(defn user-insert! [user-hash friend-hashes]
  ;; TODO: We are currently storing ID too. Just keep hash
  (try
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
         :friends friend-ids}))
    (catch Exception e (or (.getNextException e) e))))

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

(defn sell-offer-set! [user-id minval maxval]
  (try
    (sql/with-db-transaction
      [tx db]
      (sql/execute! tx ["
INSERT INTO sell_offer (user_id, min, max) VALUES (?, ?, ?)
ON CONFLICT (user_id) DO UPDATE SET min = ?, max = ?
" user-id minval maxval minval maxval])
      (log! tx "sell-offer-set" {:user-id user-id :min minval :max maxval}))
    (catch Exception e (or (.getNextException e) e))))

(defn sell-offer-get-by-user [user-id]
  (first
   (sql/query db ["
SELECT min, max FROM sell_offer WHERE user_id = ?;
" user-id])))

(defn sell-offer-unset! [user-id]
  (try
    (sql/with-db-transaction
      [tx db]
      (sql/execute! tx ["
DELETE FROM sell_offer WHERE user_id = ?;
" user-id])
      (log! tx "sell-offer-unset" {:user-id user-id}))
    (catch Exception e (or (.getNextException e) e))))

(defn get-all-sell-offers []
  (into []
        (sql/query db ["
SELECT * FROM sell_offer;
"])))

;;
;; Buy Requests
;;

(defn ->kebab-case [r] (reduce-kv #(assoc %1 (case-shift/->kebab-case %2) %3) {} r))

(defn buy-request-create! [user-id amount currency-buy currency-sell exchange-rate]
  (try
    (-> (sql/with-db-transaction
          [tx db]
          (let [buy-request (sql/query tx ["
INSERT INTO buy_request (buyer_id, amount, currency_buy, currency_sell, exchange_rate)
VALUES (?, ?, ?, ?, ?)
RETURNING *;
" user-id amount currency-buy currency-sell exchange-rate])]
            (log! tx "buy-request-create" {:user-id user-id
                                           :amount amount
                                           :currency-buy currency-buy
                                           :currency-sell currency-sell
                                           :exchange-rate exchange-rate})
            buy-request))
        first
        ->kebab-case)
    (catch Exception e (log/debug (or (.getNextException e) e)) nil)))

(defn get-buy-requests-by-user [user-id]
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM buy_request WHERE buyer_id = ?;
" user-id])))

(defn get-buy-request-by-id [buy-request]
  (-> (sql/query db ["
SELECT * FROM buy_request
WHERE id = ?;
" buy-request])
      first
      ->kebab-case))

(defn buy-request-set-seller! [buy-request seller-id]
  (try
    (sql/with-db-transaction
      [tx db]
      (sql/execute! tx ["
UPDATE buy_request SET seller_id = ?
WHERE id = ?
" seller-id buy-request])
      (log! tx "buy-request-set-seller" {:id buy-request :seller-id seller-id}))
    seller-id
    (catch Exception e (or (.getNextException e) e))))

(defn buy-request-unset-seller! [buy-request]
  (try
    (sql/execute! db ["
UPDATE buy_request SET seller_id = NULL
WHERE id = ?
" buy-request])
    (catch Exception e (or (.getNextException e) e))))

(defn buy-request-delete! [buy-request]
  (try
    (sql/execute! db ["
DELETE FROM buy_request WHERE id = ?;
" buy-request])
    'ok
    (catch Exception e (or (.getNextException e) e))))

(defn get-all-buy-requests []
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM buy_request;
"])))

;;
;; Contracts
;;

(defn contract-create!
  ([{:keys [buyer-id seller-id amount currency-buy currency-sell exchange-rate] :as params}]
   (when-not (= buyer-id seller-id) ;; TODO: check if they are friends^2
     (try
       (sql/with-db-transaction
         [tx db]
         (let [contract
               (first
                (sql/query tx ["
INSERT INTO contract (hash, buyer_id, seller_id, amount, currency_buy, currency_sell, exchange_rate)
VALUES (?, ?, ?, ?, ?, ?, ?)
RETURNING *;
" (crypto/base64 27) buyer-id seller-id amount currency-buy currency-sell exchange-rate]))]
           (sql/execute! tx ["
INSERT INTO contract_events (contract_id, stage, status) VALUES (?, ?, ?);
" (:id contract) 0 "waiting"])
           (log! tx "contract-create" params)
           contract))
       (catch Exception e (or (.getNextException e) e)))))
  ([buyer-id seller-id amount]
   (contract-create! buyer-id seller-id amount "xbt")))

(defn contract-add-event! [contract-id stage status]
  (try
    (sql/with-db-transaction
      [tx db]
      (sql/execute! tx ["
INSERT INTO contract_events (contract_id, stage, status) VALUES (?, ?, ?);
" contract-id stage status])
      (log! tx "contract-add-event" {:id contract-id :stage stage :status status}))
    (catch Exception e (or (.getNextException e) e))))

(defn get-contract-events [contract-id]
  (into []
        (sql/query db ["
SELECT time, stage, status FROM contract
INNER JOIN contract_events
ON contract.id = contract_events.contract_id;
"])))

(defn get-contract-last-event [contract-id]
  (-> (sql/query db ["
SELECT time, stage, status FROM contract
INNER JOIN contract_events
ON contract.id = contract_events.contract_id
WHERE contract_events.time = (SELECT MAX(contract_events.time) FROM contract_events);
"])
      first
      ->kebab-case))

(defn get-contracts-by-user [user-id]
  (mapv ->kebab-case
        (sql/query db ["
SELECT * FROM contract
WHERE buyer_id = ? OR seller_id = ?
" user-id user-id])))

(defn get-contract-by-id [id]
  (-> (sql/query db ["
SELECT * FROM contract
WHERE id = ?
" id])
      first
      ->kebab-case))

(defn get-all-contracts []
  (into []
        (sql/query db ["
SELECT * FROM contract
"])))

;;
;; Development utilities
;;

(defn reset-database!!! []
  (try
    (sql/db-do-commands db ["DROP TABLE IF EXISTS logs;"
                            "DROP TABLE IF EXISTS contract_events;"
                            "DROP TABLE IF EXISTS contract;"
                            "DROP TABLE IF EXISTS sell_offer;"
                            "DROP TABLE IF EXISTS buy_request;"
                            "DROP TABLE IF EXISTS friends;"
                            "DROP TABLE IF EXISTS user_account CASCADE;"
                            "
CREATE TABLE user_account (
  id              SERIAL PRIMARY KEY,
  created         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  hash            TEXT NOT NULL UNIQUE
);"
                            "
CREATE TABLE friends (
  user_id1        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE NOT NULL,
  user_id2        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE NOT NULL,
  PRIMARY KEY (user_id1, user_id2)
);"
                            "
CREATE TABLE sell_offer (
  user_id                          INTEGER REFERENCES user_account(id) ON UPDATE CASCADE NOT NULL,
  CONSTRAINT one_offer_per_user    UNIQUE (user_id),
  min                              BIGINT NOT NULL,
  max                              BIGINT NOT NULL,
  currency                         TEXT DEFAULT 'xbt' NOT NULL
);"

                            ;; TODO: CHECK THESE ON UPDATE CASCADES

                            "
CREATE TABLE buy_request (
  id                              SERIAL PRIMARY KEY,
  created                         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  buyer_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE NOT NULL,
  seller_id                       INTEGER REFERENCES user_account(id) ON UPDATE CASCADE,
  amount                          BIGINT NOT NULL,
  currency_buy                    TEXT DEFAULT 'xbt' NOT NULL,
  currency_sell                   TEXT DEFAULT 'usd' NOT NULL,
  exchange_rate                   DECIMAL(26,6)
);"
                            "
CREATE TABLE contract (
  id                              SERIAL PRIMARY KEY,
  hash                            TEXT NOT NULL UNIQUE,
  created                         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  buyer_id                        INTEGER REFERENCES user_account(id) ON UPDATE CASCADE NOT NULL,
  seller_id                       INTEGER REFERENCES user_account(id) ON UPDATE CASCADE,
  amount                          BIGINT NOT NULL,
  currency_buy                    TEXT DEFAULT 'xbt' NOT NULL,
  currency_sell                   TEXT DEFAULT 'usd' NOT NULL,
  exchange_rate                   DECIMAL(26,6)
);"
                            "
CREATE TABLE contract_events (
  id              SERIAL PRIMARY KEY,
  time            TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  contract_id     INTEGER REFERENCES contract(id) ON UPDATE CASCADE NOT NULL,
  stage           SMALLINT NOT NULL,
  status          TEXT NOT NULL
);"
                            "
CREATE TABLE logs (
  id              SERIAL PRIMARY KEY,
  time            TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  type            TEXT NOT NULL,
  data            TEXT NOT NULL
);"
                            ])
    (catch Exception e (or (.getNextException e) e))))

(defn populate-test-database!!! []
  (user-insert! "asdf" [])
  (user-insert! "ffff" ["asdf"])
  (user-insert! "aaaa" ["ffff"])
  (user-insert! "bbbb" ["asdf"])
  (user-insert! "cccc" [])
  (user-insert! "dddd" ["ffff"])
  'ok)

;; Only one entry lock:
;; lock                      CHAR(1) NOT NULL DEFAULT('X'),
;; constraint only_one       PRIMARY KEY (lock),
;; constraint CK_T1_locked   CHECK (lock='X'),
