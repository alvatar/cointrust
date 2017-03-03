(ns oracle.common)

(defn satoshi->btc [sat]
  (float (/ sat 100000000)))

(defn btc->satoshi [btc]
  (* btc 100000000))

#?(:clj
   (defn currency-as-long [amount currency]
     (let [bigdec (BigDecimal. amount)]
       (case (keyword currency)
         :btc (long (btc->satoshi bigdec))
         :usd (long (* 100 bigdec))))))

(defn currency-as-float [amount currency]
  (case (keyword currency)
    :btc (float (satoshi->btc amount))
    :usd (float (/ amount 100.0))))

(defn long->incr [val]
  (+ 1 (/ val 10000)))

(defn long->decr [val]
  (- 1 (/ val 10000)))
