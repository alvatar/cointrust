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

(defn round-currency
  ([val arg]
   (if (number? arg)
     (let [pow (#?(:clj Math/pow)
                #?(:cljs js/Math.pow)
                10 arg)]
       (/ (long (* pow val)) pow))
     (round-currency val (case (keyword arg)
                           :usd 2
                           :btc 8
                           8))))
  ([val]
   (round-currency val 2)))

(defn currency-as-float [amount currency]
  (case (keyword currency)
    :btc (float (round-currency (satoshi->btc amount) :btc))
    :usd (float (round-currency (/ amount 100.0) :usd))))

(defn currency->symbol [currency]
  (case (keyword currency)
    :btc "Ƀ"
    :usd "$"))

(defn humanize-currency [amount currency]
  (case (keyword currency)
    :btc (str amount " Bitcoin")
    :usd (str "$" amount)))

(defn long->incr [val]
  (float (+ 1 (/ val 10000))))

(defn long->decr [val]
  (float (- 1 (/ val 10000))))

;;
;; Testing utils
;;

(def fake-users
  [{:user-hash "dddd" :user-id 1 :user-name "Zeus" :fb-id 10213129106885586 :friend-hashes ["eeee"]}
   {:user-hash "eeee" :user-id 2 :user-name "Barbara" :fb-id 10106263879382352 :friend-hashes ["dddd"]}])
