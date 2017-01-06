(ns oracle.database
  (:gen-class)
  (:require [environ.core :refer [env]]
            [clojure.java.jdbc :as sql]
            [crypto.random :as crypto]
            [clojure.data.json :as json]))

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
  ;; TODO: We are currently storing
  (try
    (sql/with-db-transaction
      [tr db]
      (let [friend-ids
            (mapv :id
                  (first
                   (for [f friend-hashes]
                     (sql/query db ["
SELECT id FROM users WHERE hash = ?;
" f]))))
            user-id
            (-> (sql/query db ["
INSERT INTO users (hash) VALUES (?)
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
                       (sql/execute! tr ["
INSERT INTO friends (user_id1, user_id2)
VALUES (?, ?)
ON CONFLICT DO NOTHING
" user-id1 user-id2]))))
                  friend-ids)]
        (log! tr "user-insert" {:user-hash user-hash
                              :friend-hashes friend-hashes})
        {:user user-id
         :friends friend-ids}))
    (catch Exception e (or (.getNextException e) e))))

(defn get-users []
  (sql/query db ["SELECT * FROM users;"]))

(defn get-friends []
  (sql/query db ["SELECT * FROM friends;"]))

(defn get-user-by-id [id]
  (-> (sql/query db ["SELECT hash FROM users WHERE id = ?;" id])
      first
      :hash))

(defn get-user-by-hash [hash]
  (-> (sql/query db ["SELECT id FROM users WHERE hash = ?;" hash])
      first
      :id))

(defn get-user-friends [id]
  (sql/query db ["
SELECT user_id2 AS user FROM users, friends
WHERE user_id1 = ? AND id = ?
UNION
SELECT user_id1 AS user FROM users, friends
WHERE user_id2 = ? AND id = ?
" id id id id]))

(defn get-user-edges [id]
  (sql/query db ["
SELECT user_id1, user_id2 FROM users, friends
WHERE (user_id1 = ? OR user_id2 = ?) AND id = ?
" id id id]))

(defn get-user-friends-of-friends [id]
  (mapv :user
        (sql/query db ["
WITH user_friends AS (
          SELECT user_id2 AS user FROM users, friends
          WHERE user_id1 = ? AND id = ?
          UNION
          SELECT user_id1 AS user FROM users, friends
          WHERE user_id2 = ? AND id = ?
     )
SELECT user_id1 AS user FROM users, friends
WHERE NOT (user_id1 = ?) AND id IN (SELECT * FROM user_friends)
UNION
SELECT user_id2 AS user FROM users, friends
WHERE NOT (user_id2 = ?) AND id IN (SELECT * FROM user_friends)
" id id id id id id])))

;;
;; Sell Offers
;;

(defn sell-offer-set! [user-id minval maxval]
  (try
    (sql/execute! db ["
INSERT INTO sell_offer (user_id, min, max) VALUES (?, ?, ?)
ON CONFLICT (user_id) DO UPDATE SET min = ?, max = ?
" user-id minval maxval minval maxval])
    'ok
    (catch Exception e (or (.getNextException e) e))))

(defn sell-offer-get-by-user [user-id]
  (first
   (sql/query db ["
SELECT min, max FROM sell_offer WHERE user_id = ?;
" user-id])))

(defn sell-offer-unset! [user-id]
  (try
    (sql/execute! db ["
DELETE FROM sell_offer WHERE user_id = ?;
" user-id])
    'ok
    (catch Exception e (or (.getNextException e) e))))

(defn get-all-sell-offers []
  (into []
        (sql/query db ["
SELECT * FROM sell_offer;
"])))

;;
;; Buy Requests
;;

(defn buy-request-set! [user-id val]
  (try
    (sql/execute! db ["
INSERT INTO buy_request (user_id, val) VALUES (?, ?)
ON CONFLICT (user_id) DO UPDATE SET val = ?
" user-id val val])
    'ok
    (catch Exception e (or (.getNextException e) e))))

(defn buy-request-get-by-user [user-id]
  (first
   (sql/query db ["
SELECT val FROM buy_request WHERE user_id = ?;
" user-id])))

(defn buy-request-unset! [user-id]
  (try
    (sql/execute! db ["
DELETE FROM buy_request WHERE user_id = ?;
" user-id])
    'ok
    (catch Exception e (or (.getNextException e) e))))

(defn get-all-buy-requests []
  (into []
        (sql/query db ["
SELECT * FROM buy_request;
"])))

;;
;; Contracts
;;

(defn contract-create! [buyer-id seller-id btc-amount]
  (when-not (= buyer-id seller-id) ;; TODO: check if they are friends^2
    (try
      (sql/with-db-transaction
        [tx db]
        (let [contract
              (first
               (sql/query tx ["
INSERT INTO contracts (hash, buyer, seller, btc) VALUES (?, ?, ?, ?) RETURNING *;
" (crypto/base64 27) buyer-id seller-id btc-amount]))]
          (sql/execute! tx ["
INSERT INTO contract_events (contract_id, stage, status) VALUES (?, ?, ?);
" (:id contract) 0 "waiting"])
          (log! tx "user-insert" {:buyer-id buyer-id
                                :seller-id seller-id
                                  :btc-amount btc-amount})
          contract))
      (catch Exception e (or (.getNextException e) e)))))

(defn contract-add-event! [contract-id stage status]
  (try
    (sql/with-db-transaction
      [tx db]
      (sql/execute! tx ["
INSERT INTO contract_events (contract_id, stage, status) VALUES (?, ?, ?);
" contract-id stage status]))
    (catch Exception e (or (.getNextException e) e))))

(defn get-contract-events [contract-id]
  (try
    (into []
          (sql/query db ["
SELECT time, stage, status FROM contracts
INNER JOIN contract_events
ON contracts.id = contract_events.contract_id;
"]))
    (catch Exception e (or (.getNextException e) e))))

(defn get-contract-last-event [contract-id]
  (try
    (first
     (sql/query db ["
SELECT time, stage, status FROM contracts
INNER JOIN contract_events
ON contracts.id = contract_events.contract_id
WHERE contract_events.time = (SELECT MAX(contract_events.time) FROM contract_events);
"]))
    (catch Exception e (or (.getNextException e) e))))

(defn get-user-contracts [user-id]
  (try
    (into []
          (sql/query db ["
SELECT * FROM contracts
WHERE buyer = ? OR seller = ?
" user-id user-id]))
    (catch Exception e (or (.getNextException e) e))))

(defn get-all-contracts []
  (sql/query db ["
SELECT * FROM contracts
"]))

;;
;; Development utilities
;;

(defn reset-database!!! []
  (try
    (sql/db-do-commands db ["DROP TABLE IF EXISTS logs;"
                            "DROP TABLE IF EXISTS contract_events;"
                            "DROP TABLE IF EXISTS contracts;"
                            "DROP TABLE IF EXISTS sell_offer;"
                            "DROP TABLE IF EXISTS buy_request;"
                            "DROP TABLE IF EXISTS friends;"
                            "DROP TABLE IF EXISTS users;"
                            "
CREATE TABLE users (
  id              SERIAL PRIMARY KEY,
  created         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  hash            TEXT NOT NULL UNIQUE
);"
                            "
CREATE TABLE friends (
  user_id1        INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  user_id2        INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  PRIMARY KEY (user_id1, user_id2)
);"
                            "
CREATE TABLE sell_offer (
  user_id                          INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  CONSTRAINT one_offer_per_user    UNIQUE (user_id),
  min                              BIGINT NOT NULL,
  max                              BIGINT NOT NULL
);"
                            "
CREATE TABLE buy_request (
  user_id                           INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  CONSTRAINT one_request_per_user   UNIQUE (user_id),
  amount                            BIGINT NOT NULL
);"
                            "
CREATE TABLE contracts (
  id              SERIAL PRIMARY KEY,
  hash            TEXT NOT NULL UNIQUE,
  buyer           INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  seller          INTEGER REFERENCES users(id) ON UPDATE CASCADE NOT NULL,
  btc             TEXT NOT NULL
);"
                            "
CREATE TABLE contract_events (
  id              SERIAL PRIMARY KEY,
  time            TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  contract_id     INTEGER REFERENCES contracts(id) ON UPDATE CASCADE NOT NULL,
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
