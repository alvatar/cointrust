(ns oracle.common)

(defn satoshi->btc [sat]
  (/ sat 100000000))

(defn btc->satoshi [btc]
  (* btc 100000000))

#?(:clj
   (defn currency-as-long [amount currency]
     (let [bigdec (BigDecimal. amount)]
       (case (keyword currency)
         :xbt (long (btc->satoshi bigdec))
         :usd (long (* 100 bigdec))))))

(defn currency-as-float [amount currency]
  (case (keyword currency)
    :xbt (float (satoshi->btc amount))
    :usd (float (/ amount 100.0))))
