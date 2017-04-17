(ns oracle.globals)

(goog-define *env* "dev")
(def hook-fake-id?_ (or (= *env* "dev") (= *env* "test")))

(def exchange-rates-refresh-interval 60)
(def max-allowed-transaction-in-usd 2500)
