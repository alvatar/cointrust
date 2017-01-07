(ns oracle.common)

(defn satoshi->btc [sat]
  (/ sat 100000000))

(defn btc->satoshi [btc]
  (* btc 100000000))

#?(:clj
   (defn currency-as-long [amount currency]
     (let [bigdec (BigDecimal. amount)]
       (case (keyword currency)
         :xbt (long (btc->satoshi bigdec))))))

(defn currency-as-decimal [amount currency]
  (case (keyword currency)
    :xbt (float (satoshi->btc amount))))
