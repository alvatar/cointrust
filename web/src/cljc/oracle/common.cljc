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

(defn currency->symbol [currency]
  (case (keyword currency)
    :btc "Ƀ"
    :usd "$"))

(defn humanize-currency [amount currency]
  (case (keyword currency)
    :btc (str amount " Bitcoin")
    :usd (str "$" amount)))

(defn long->incr [val]
  (+ 1 (/ val 10000)))

(defn long->decr [val]
  (- 1 (/ val 10000)))

#?(:cljs
   (defn round-currency
     ([val num-decimals]
      (let [pow (js/Math.pow 10 num-decimals)]
        (/ (long (* pow val)) pow)))
     ([val]
      (round-currency val 5))))
