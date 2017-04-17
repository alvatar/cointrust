(ns oracle.common)

(defn satoshi->btc [sat]
  (float (/ sat 100000000)))

#?(:clj
   (defn btc->satoshi [btc]
     (long (* btc 100000000))))

#?(:cljs
   (defn btc->satoshi [btc]
     (* btc 100000000)))

#?(:clj
   (defn currency-as-long [amount currency]
     (let [bigdec (BigDecimal. amount)]
       (case (keyword currency)
         :btc (long (btc->satoshi bigdec))
         :usd (long (* 100 bigdec))))))

(defn pow [x n]
  (#?(:clj Math/pow)
   #?(:cljs js/Math.pow)
   x n))

(defn round-currency
  ([val arg]
   (if (number? arg)
     (let [power (pow 10 arg)]
       (/ (long (* power val)) power))
     (round-currency val (case (keyword arg)
                           :usd 2
                           :btc 8
                           8))))
  ([val]
   (round-currency val 2)))

(defn currency-as-floating-point [amount currency]
  (case (keyword currency)
    :btc (double (round-currency (satoshi->btc amount) :btc))
    :usd (double (round-currency (/ amount 100.0) :usd))))

(defn currency->symbol [currency]
  (case (keyword currency)
    :btc "Ƀ"
    :usd "$"))

(defn humanize-currency [amount currency]
  (case (keyword currency)
    :btc (str amount " Bitcoin")
    :usd (str "$" amount)))

(defn currency-tax [val tax decimals]
  (long (* (+ 1 (/ tax (pow 10 (+ 2 decimals))))
           val)))

(defn currency-discount [val discount decimals]
  (long (* (- 1 (/ discount (pow 10 (+ 2 decimals))))
           val)))

;;
;; Testing utils
;;

(def fake-users
  [{:user-hash "dddd" :user-id 1 :user-name "Zeus" :fb-id 10213129106885586 :friend-hashes ["eeee"]}
   {:user-hash "eeee" :user-id 2 :user-name "Barbara" :fb-id 10106263879382352 :friend-hashes ["dddd"]}])
