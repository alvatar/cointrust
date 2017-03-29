(ns oracle.components
  (:require [taoensso.sente :as sente :refer (cb-success?)]
            [rum.core :as rum]
            [cljs-react-material-ui.core :refer [get-mui-theme color] :as muic]
            [cljs-react-material-ui.icons :as ic]
            [cljs-react-material-ui.rum :as ui]
            [goog.string :as gstring]
            ;; -----
            [oracle.common :as common]
            [oracle.globals :as globals]
            [oracle.utils :as utils]
            [oracle.state :as state]
            [oracle.network :as network]
            [oracle.actions :as actions]))


;;
;; Globals
;;

(defonce window (atom {:width (aget js/window "innerWidth")
                       :height (aget js/window "innerHeight")}))
(defonce small-display? (atom (< (:width @window) 780)))
(defonce _small-display-check
  (. js/window addEventListener "resize"
     (fn []
       (let [width (aget js/window "innerWidth")
             height (aget js/window "innerHeight")]
         (swap! window assoc :width width)
         (swap! window assoc :height height)
         (reset! small-display? (< width 780))))))

(def smart-contract-fields
  {:aria-label "Smart contracts are computer protocols that facilitate, verify, or enforce the negotiation or performance of a contract, or that make a contractual clause unnecessary. Smart contracts often emulate the logic of contractual clauses."} )

;;
;; Helpers
;;

(defn am-i-buyer? [contract] (= @(:user-id state/app) (:buyer-id contract)))
(defn am-i-seller? [contract] (= @(:user-id state/app) (:seller-id contract)))
(defn waiting-transfer? [contract] (= (:stage contract) "waiting-transfer"))

(defn contract-time-left [contract server-time]
  (let [milliseconds->mins-formatted
        (fn [mil]
          (let [secs-raw (long (/ mil 1000))
                secs (mod secs-raw 60)
                mins-tot (quot secs-raw 60)
                hours (quot mins-tot 60)
                mins (mod mins-tot 60)]
            (if (pos? secs-raw)
              (if (zero? hours)
                (gstring/format "%s min. %s sec." mins secs)
                (gstring/format "%sh %sm %ss" hours mins secs))
              "no time")))]
    (case (:stage contract)
      "waiting-start" ;; 12 hours
      (milliseconds->mins-formatted (- (* 12 60 60 1000)
                                       (- server-time (:created contract))))
      "waiting-escrow" ;; 60 mins
      (milliseconds->mins-formatted (- (* 60 60 1000)
                                       (- server-time (:started-timestamp contract))))
      "waiting-transfer" ;; 30 mins
      (milliseconds->mins-formatted (- (* 30 60 1000)
                                       (- server-time (:escrow-funded-timestamp contract))))
      0)))

(defn close-display-contract! [] (reset! (:display-contract state/app) nil))

;;
;; Widgets
;;

(rum/defc top-notification-bar
  < rum/reactive
  []
  (let [seconds-until (rum/react (:seconds-until-next-reconnect state/app))]
    (when seconds-until
      [:div.top-notification-bar.fadeIn
       {:style {:position "fixed"
                :top 0 :left 0 :right 0
                :height "3rem"
                :line-height "3rem"
                :background-color "rgb(0, 188, 212)"
                :z-index 99999}}
       (gstring/format "Disconnected. Trying to reconnect in %s seconds..." seconds-until)])))

(defn mobile-overlay [open? & children]
  [:div {:style {:position "fixed"
                 :top 0 :left 0 :right 0 :bottom 0
                 :background-color "#fff"
                 :z-index 9999
                 :display (if open? "block" "none")}}
   (into [:div {:style {:position :static}}]
         children)])

;;
;; UI Components
;;

(rum/defcs buy-dialog
  < rum/reactive (rum/local {:amount 0.1 :currency "btc"} ::input)
  [state]
  (let [input (::input state)
        exchange-rates (rum/react (:exchange-rates state/app))
        parsed-val (js/parseFloat (:amount (rum/react input)))
        open? (= (rum/react (:ui-mode state/app)) :buy-dialog)
        currency (:currency (rum/react input))
        current-by-currency #(case (:currency @input)
                               "btc" (case %
                                       :btc parsed-val
                                       :usd (common/round-currency (* (:btc-usd exchange-rates) parsed-val) :usd))
                               "usd" (case %
                                       :usd (common/round-currency parsed-val :usd)
                                       :btc (* (:usd-btc exchange-rates) parsed-val)))
        content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                 [:div [:h4 "Price: 1 Bitcoin = $" (:btc-usd exchange-rates)]
                  [:h6 {:style {:margin-top "-1rem"}}
                   (gstring/format "Exchange rate will update in %s seconds" (- globals/exchange-rates-refresh-interval
                                                                                (rum/react (:seconds-since-last-exchange-rates-refresh state/app))))]
                  [:div
                   (ui/select-field {:value currency
                                     :floating-label-text "Currency"
                                     :on-change (fn [ev idx val] (swap! input assoc :currency val))
                                     :style {:width "8rem"}}
                                    (ui/menu-item {:value "usd" :primary-text "USD"})
                                    (ui/menu-item {:value "btc" :primary-text "BTC"}))]
                  (ui/text-field {:id "amount"
                                  :style {:width "10rem" :margin-right "1rem"}
                                  :value (:amount (rum/react input))
                                  :on-change #(swap! input assoc :amount (.. % -target -value))
                                  :error-text (when-not (and parsed-val (pos? parsed-val)) "Invalid value")})
                  (when (and parsed-val (pos? parsed-val))
                    (case currency
                      "btc" (gstring/format "for %s USD" (common/round-currency (* (:btc-usd exchange-rates) parsed-val) :usd))
                      "usd" (gstring/format "gets you %s BTC" (common/round-currency (* (:usd-btc exchange-rates) parsed-val) :btc))))
                  [:h6 {:style {:margin-top "0.5rem"}} "Note: 1% fee and 1% seller premium will be paid from the total purchased."]
                  [:h4 {:style {:margin-top "-1rem"}}
                   (gstring/format "You will be paying $%s and receiving %s Bitcoin"
                                   (current-by-currency :usd)
                                   (common/round-currency (* 0.99 0.99 (current-by-currency :btc)) :btc))]]
                 (when (:processing (rum/react input))
                   [:div
                    (ui/linear-progress {:size 60 :mode "indeterminate"
                                         :style {:margin "auto" :left "0" :right "0" :top "1.5rem"}})
                    [:h5 {:style {:text-align "center" :margin-top "2rem"}} "Initiating contract"]])]
        buttons [(ui/flat-button {:label "Buy"
                                  :primary true
                                  :disabled (or (:processing (rum/react input)) (not (and parsed-val (pos? parsed-val))))
                                  :on-touch-tap
                                  (fn [e] (when (and parsed-val (pos? parsed-val))
                                            (swap! input assoc :processing true)
                                            (actions/create-buy-request (current-by-currency :btc)
                                                                        #(do (reset! (:ui-mode state/app) :none)
                                                                             (swap! input assoc :processing false)))))})
                 (ui/flat-button {:label "Cancel"
                                  :on-touch-tap #(reset! (:ui-mode state/app) :none)})]]
    (if (>= (count @(:buy-requests state/app)) 50)
      (ui/dialog {:title "Maximum number reached"
                  :open open?
                  :actions [(ui/flat-button {:label "OK"
                                             :on-touch-tap #(reset! (:ui-mode state/app) :none)})]}
                 "Maximum number of simultaneous open BUY requests reached.")
      (if (rum/react small-display?)
        (mobile-overlay open? content (into [:div] buttons))
        (ui/dialog {:title "Buy Bitcoins"
                    :open open?
                    :modal true
                    :content-style {:max-width "500px"}
                    :actions buttons}
                   content)))))

;; (rum/defc sell-slider [currency [min-val max-val] on-change]
;;   (js/React.createElement js/Slider #js {:min 1
;;                                          :max 20000
;;                                          :range true
;;                                          :allowCross false
;;                                          :value #js [min-val max-val]
;;                                          :tipFormatter nil
;;                                          :onChange on-change}))

(rum/defcs sell-dialog
  < rum/reactive (rum/local {} ::ui-values)
  [state_]
  (let [offer-active? (boolean @(:sell-offer state/app))
        ui-values (::ui-values state_)
        sell-offer (:sell-offer state/app)
        min-val (or (:min (rum/react ui-values)) (:min (rum/react sell-offer)) 0.1)
        max-val (or (:max (rum/react ui-values)) (:max (rum/react sell-offer)) globals/max-allowed-transaction-in-usd)
        parsed-min-val (let [p (js/Number min-val)] (when-not (js/isNaN p) p))
        parsed-max-val (let [p (js/Number max-val)] (when-not (js/isNaN p) p))
        currency (or (:currency (rum/react ui-values)) (:currency (rum/react sell-offer)) "usd")
        ex-rate (get (rum/react (:exchange-rates state/app)) (if (= currency "usd") :usd-btc :btc-usd))
        open? (= (rum/react (:ui-mode state/app)) :sell-dialog)
        content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                 [:div
                  [:h4 "How much would you like to sell? (Your coins will be sold at the market price, as listed on the top US exchange)"]
                  [:h4 "Bitcoin price: $1" (:btc-usd (rum/react (:exchange-rates state/app)))]
                  [:h6 {:style {:margin-top "-1rem"}}
                   (gstring/format "Exchange rate will update in %s seconds" (- globals/exchange-rates-refresh-interval
                                                                                (rum/react (:seconds-since-last-exchange-rates-refresh state/app))))]
                  (ui/select-field {:value currency
                                    :floating-label-text "Currency"
                                    :on-change (fn [ev idx val] (swap! ui-values assoc :currency val))
                                    :style {:width "8rem"}}
                                   (ui/menu-item {:value "usd" :primary-text "USD"})
                                   (ui/menu-item {:value "btc" :primary-text "BTC"}))]
                 [:div.group
                  [:div {:style {:float "left"}}
                   (ui/text-field {:id "min"
                                   :floating-label-text "Min."
                                   :style {:margin-top "0px" :width "8rem"}
                                   :error-text (cond (neg? parsed-min-val) "Invalid value"
                                                     (< (js/Math.abs (- parsed-max-val parsed-min-val)) (case currency "usd" 1 "btc" 0.001))
                                                     "We recommend using a wider range")
                                   :value min-val
                                   :on-change #(swap! ui-values assoc :min (.. % -target -value))})]
                  (when parsed-min-val
                    [:div {:style {:float "left" :width "50%" :margin-top "2.5rem"}}
                     (clojure.string/upper-case currency) " - ("
                     (if (= currency "btc")
                       (str (common/round-currency (* ex-rate parsed-min-val) :usd) " USD)")
                       (str (common/round-currency (* ex-rate parsed-min-val) :btc) " BTC)"))])]
                 [:div.group
                  [:div {:style {:float "left"}}
                   (ui/text-field {:id "max"
                                   :floating-label-text "Max."
                                   :style {:margin-top "0px" :width "8rem"}
                                   :error-text (cond (not (pos? parsed-max-val)) "Invalid value"
                                                     (> parsed-min-val parsed-max-val) "Max should be larger than Min"
                                                     (> (case currency "usd" parsed-max-val (* ex-rate parsed-max-val)) globals/max-allowed-transaction-in-usd)
                                                     (str "Max. of " globals/max-allowed-transaction-in-usd " USD"))
                                   :value max-val
                                   :on-change #(swap! ui-values assoc :max (.. % -target -value))})]
                  (when parsed-max-val
                    [:div {:style {:float "left" :width "50%" :margin-top "2.5rem"}}
                     (clojure.string/upper-case currency) " - ("
                     (if (= currency "btc")
                       (str (common/round-currency (* ex-rate parsed-max-val) :usd) " USD)")
                       (str (common/round-currency (* ex-rate parsed-max-val) :btc) " BTC)"))])]]
        buttons [(when offer-active?
                   (ui/flat-button {:label "Remove"
                                    :key "offer-remove-button"
                                    :on-touch-tap (fn []
                                                    (when (js/confirm "Are you sure?")
                                                      (actions/close-sell-offer
                                                       #(reset! (:ui-mode state/app) :none))))}))
                 (ui/flat-button {:label "Back"
                                  :key "offer-back-button"
                                  :on-touch-tap #(reset! (:ui-mode state/app) :none)})
                 (ui/flat-button {:label (if offer-active? "Update" "Sell")
                                  :primary true
                                  :key "offer-update-button"
                                  :disabled (not (and (pos? parsed-min-val) (pos? parsed-max-val)
                                                      (<= (case currency "usd" parsed-max-val (* ex-rate parsed-max-val)) 3000)
                                                      (> parsed-max-val parsed-min-val)))
                                  :on-touch-tap (fn []
                                                  (actions/open-sell-offer {:currency currency :min min-val :max max-val :premium 0.01})
                                                  (reset! (:ui-mode state/app) :none))})]]
    (if (rum/react small-display?)
      (mobile-overlay open? [:div [:div {:style {:height "1rem"}}] content [:div {:style {:height "2rem"}}] buttons])
      (ui/dialog {:title (if offer-active? "Active offer" "Sell Bitcoin")
                  :open open?
                  :modal true
                  :content-style {:max-width "400px"}
                  :actions buttons}
                 content))))

(rum/defc menu-controls-comp
  < rum/reactive
  []
  [:div.main-menu
   [:h5.center (str "You can trade with " (count (rum/react (:friends2 state/app))) " people in your friend network")]
   [:div.center {:style {:margin-top "-1rem" :margin-bottom "1rem"}}
    (for [{:keys [id name photo-url]} (rum/react (:friends2 state/app))]
      [:span.profile-image.hint--bottom {:key (str "friend-photo-" id) :aria-label name}
       [:figure
        [:img.circle {:src photo-url}]]])]
   (when-let [error (rum/react state/error)] [:h5.center.error error])
   [:div.center
    (ui/raised-button {:label "BUY Bitcoins"
                       :disabled false #_(not (let [contracts (rum/react (:contracts state/app))]
                                                (or (= contracts :unknown) (empty? contracts))))
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode state/app) :buy-dialog)})
    (ui/raised-button {:label (if (rum/react (:sell-offer state/app)) "Change sell offer" "SELL Bitcoins")
                       :disabled false
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode state/app) :sell-dialog)})]])

(rum/defc sell-offer-comp
  < rum/reactive
  []
  (when-let [sell-offer (rum/react (:sell-offer state/app))]
    (let [currency (:currency sell-offer)
          ex-rate (get (rum/react (:exchange-rates state/app)) (if (= currency "usd") :usd-btc :btc-usd))]
      [:div
       [:h4 {:style {:text-align "center"}} "Sell offer"]
       [:p.center [:strong (:min sell-offer)] " to " [:strong (:max sell-offer)] " "
        (clojure.string/upper-case currency) " ("
        (common/round-currency (* (:min sell-offer) ex-rate) currency) " - "
        (common/round-currency (* (:max sell-offer) ex-rate) currency)
        " " (if (= currency "usd") "BTC" "USD") ")"
        [:br]]
       [:h6.center {:style {:margin-top "-0.8rem" :margin-bottom "1rem"}}
        "conversion excluding " (:premium sell-offer) "% premium"]])))

(rum/defc request-listing-comp
  < rum/reactive
  []
  (let [requests (rum/react (:buy-requests state/app))]
    (cond
      (nil? requests)
      [:div "Retrieving requests..."
       (ui/linear-progress {:size 60 :mode "indeterminate"})]
      (not-empty requests)
      [:div
       [:h4 {:style {:text-align "center"}} "Open requests"]
       [:div
        (ui/list
         (for [req requests]
           (ui/list-item {:key (str "buy-request-item-" (:id req))
                          :disabled true
                          :primary-text (str "Buy request for " (common/humanize-currency (common/currency-as-float (:amount req) (:currency-seller req))
                                                                                          (:currency-seller req)))
                          :secondary-text (gstring/format "ID: %d - %s"
                                                          (:id req)
                                                          (if (:seller-id req)
                                                            "SELLER FOUND - WAITING ON SELLER"
                                                            "LOOKING FOR A SELLER..."))})))]])))

(rum/defcs contract-dialog
  < rum/reactive
  (rum/local {:output-address "" :key ""} ::input)
  (rum/local nil ::user-key)
  (rum/local {} ::errors)
  [_state]
  (let [input (::input _state)
        user-key (::user-key _state)
        errors (::errors _state)
        contract-id (rum/react (:display-contract state/app))
        server-time (rum/react (:server-time state/app))]
    (when-let [contract (some #(and (= (:id %) contract-id) %) (rum/react (:contracts state/app)))]
      (let [time-left (contract-time-left contract server-time)
            escrow-release-dialog
            (fn [role]
             (let [buttons
                   [(ui/flat-button {:label "Ok"
                                     :key (str "contract-ok-button-" (:id contract))
                                     :primary true
                                     :on-touch-tap
                                     (fn []
                                       (if (or (= (:output-address @input) "") (= (:key @input) ""))
                                         (do (when (= (:output-address @input) "") (swap! errors assoc :output-address "Missing parameter"))
                                             (when (= (:key @input) "") (swap! errors assoc :key "Missing parameter")))
                                         (network/send!
                                          [:escrow/release-to-user {:id (:id contract)
                                                                    :user-role (name role)
                                                                    :output-address (:output-address @input)
                                                                    :escrow-user-key (:key @input)}]
                                          5000
                                          #(if (sente/cb-success? %)
                                             (case (:status %)
                                               :ok
                                               (do (actions/update-contract
                                                    (:id contract)
                                                    (fn [c] (-> c
                                                                (assoc :output-address (:output-address @input))
                                                                (assoc :escrow-release "<processing>"))))
                                                   (close-display-contract!))
                                               :error-wrong-key
                                               (swap! errors assoc :key "Invalid Key")
                                               :error-wrong-address
                                               (swap! errors assoc :output-address "Invalid address")
                                               :error-missing-parameters
                                               (utils/log* "Error calling contract/release-escrow-buyer" %)
                                               :error-escrow-not-funded
                                               (swap! errors assoc :output-address "Smart Contract not funded or not enough confirmations.")
                                               (do (swap! errors assoc :key "Unknown Error")
                                                   (swap! errors assoc :output-address "Unknown Error")
                                                   (utils/log* %)))
                                             (do (reset! state/error "There was an error in requesting the Smart Contract funds. Please inform us of this event.")
                                                 (utils/log* "Error calling contract/release-escrow-buyer" %))))))})
                    (ui/flat-button {:label "Cancel"
                                     :key (str "contract-cancel-" contract-id)
                                     :primary false
                                     :on-touch-tap close-display-contract!})]
                   content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                            [:h3 "Claim funds"]
                            [:h5 "Please provide the Destination Address and the Smart Contract Release Key in order to receive your Bitcoins"]
                            (ui/text-field {:id "output-address"
                                            :floating-label-text "Destination Address"
                                            :error-text (:output-address (rum/react errors))
                                            :value (:output-address (rum/react input))
                                            :style {:width "100%"}
                                            :on-change #(do (reset! errors {})
                                                            (swap! input assoc :output-address (.. % -target -value)))})
                            [:br]
                            (ui/text-field {:id "key"
                                            :floating-label-text "Smart Contract Release Key"
                                            :error-text (:key (rum/react errors))
                                            :value (:key (rum/react input))
                                            :style {:width "100%"}
                                            :on-change #(do (reset! errors {})
                                                            (swap! input assoc :key (.. % -target -value)))})]]
               (if (rum/react small-display?)
                 (mobile-overlay true content buttons)
                 (ui/dialog {:title (str "Smart Contract ID: " (:human-id contract))
                             :open true
                             :modal true
                             :actions buttons}
                            content))))]

        (case (:stage contract)

          "waiting-start"
          (let [buttons [(ui/flat-button {:label "Close"
                                          :key (str "ws-close-button-contract-" contract-id)
                                          :primary true
                                          :on-touch-tap close-display-contract!})]
                the-other (if (am-i-seller? contract) (:buyer-name contract) (:seller-name contract))
                content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                         [:div (if (:escrow-seller-has-key contract) {:style {:color "#bbb"}} {})
                          [:h3 (str "Time left: " time-left)]
                          [:p (gstring/format "You must coordinate a start-time for this trade with %s on Facebook Messenger within the time left." the-other)]
                          [:p (if (am-i-seller? contract)
                                (gstring/format "Once started you’ll have 60 minutes to send the bitcoin to a smart contract and 30 minutes to confirm you’ve received payment from %s." the-other)
                                (gstring/format "Once started you'll have to wait for a maximum of 60 minutes for %s to send the Bitcoins to the smart contract, and then you will have 30 minutes to make the payment." the-other))]]
                         (when (am-i-seller? contract)
                           (ui/raised-button {:label "Start Trade"
                                              :key (str "ws-start-interaction-button-contract" contract-id)
                                              :primary true
                                              :on-touch-tap (fn []
                                                              (actions/contract-start contract-id)
                                                              (close-display-contract!))}))]]
            (if (rum/react small-display?)
              (mobile-overlay true content buttons)
              (ui/dialog {:title (str "Smart Contract ID: " (:human-id contract))
                          :open (boolean contract)
                          :modal true
                          :content-style {:width "500px"}
                          :actions buttons}
                         content)))

          "waiting-escrow"
          (let [buttons [(ui/flat-button {:label "Close"
                                          :key (str "we-close-button-contract-" contract-id)
                                          :primary true
                                          :on-touch-tap close-display-contract!})]
                content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                         [:div (if (:escrow-seller-has-key contract) {:style {:color "#bbb"}} {})
                          [:h3 (str "Time left: " time-left)]
                          [:h3 "Step 1: Get the Smart Contract key"]
                          (if (:escrow-seller-has-key contract)
                            [:p "The key has been extracted and is no longer available in our servers."]
                            [:div
                             [:div.center
                              (if (and (nil? (rum/react user-key)) (not (:escrow-seller-has-key contract)))
                                (ui/raised-button {:label "Get the Smart Contract Key" :primary true
                                                   :on-touch-tap #(actions/escrow-retrieve-key contract-id :seller user-key)})
                                (ui/raised-button {:label "I have stored my key in a secure place" :primary true
                                                   :on-touch-tap #(when (js/confirm "Please double-check the key. You will not be able to recover your funds without it.")
                                                                    (actions/escrow-forget-key contract-id :seller))}))]
                             [:div.center.margin-2rem
                              [:div.center {:style {:font-size "small"}} (rum/react user-key)]]])]
                         [:h3 "Step 2: send " [:span {:style {:color "rgb(0, 188, 212)"}}
                                               (* (common/currency-as-float (:amount contract) (:currency-seller contract))
                                                  (common/long->decr (:premium contract)))
                                               " " (clojure.string/upper-case (:currency-seller contract))] " to the following Smart Contract address"]
                         [:div {:style {:background-color "#fff" :border-radius "2px"}}
                          [:div {:style {:color "#000" :padding "10px 0px 10px 0px" :text-align "center"}}
                           (:input-address contract)]]]]
            (when (am-i-seller? contract) ; make sure we are the seller
              (if (rum/react small-display?)
                (mobile-overlay true content buttons)
                (ui/dialog {:title (str "Smart Contract ID: " (:human-id contract))
                            :open (boolean contract)
                            :modal true
                            :content-style {:width "500px"}
                            :actions buttons}
                           content))))

          "waiting-transfer"
          (let [legal-text (fn [role]
                             [:p.special-text
                              "My name is "
                              (case role :buyer "<say your legal name>." :seller "<his/her legal name>")
                              [:br]
                              "My name on Facebook is "
                              (case role :buyer "<say your Facebook name>" :seller "<his/her name on Facebook>")
                              [:br]
                              (gstring/format "This is a video contract confirming that I am conditionally agreeing to purchase %s Bitcoins."
                                              (common/currency-as-float (:amount contract) (:currency-seller contract)))
                              [:br]
                              (gstring/format "As long as the contract ID '%s' listed at cointrust.io shows that the contract is completed when I log into CoinTrust with my Facebook ID." (:human-id contract))])
                content
                (if (am-i-buyer? contract)
                  ;; Buyer dialog
                  [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                   [:div (if (:escrow-buyer-has-key contract) {:style {:color "#bbb"}} {})
                    [:h3 "Step 1: get the Smart Contract key"]
                    (if (:escrow-buyer-has-key contract)
                      [:p "The key has been extracted and is no longer available in our servers."]
                      [:div
                       [:div.center
                        (if (and (nil? (rum/react user-key)) (not (:escrow-buyer-has-key contract)))
                          (ui/raised-button {:label "Get the Smart Contract Key"
                                             :primary true
                                             :on-touch-tap #(actions/escrow-retrieve-key contract-id :buyer user-key)})
                          (ui/raised-button {:label "I have stored my key in a secure place"
                                             :primary true
                                             :on-touch-tap #(when (js/confirm "Please double-check the key. You will not be able to recover your funds without it.")
                                                             (actions/escrow-forget-key contract-id :buyer))}))]
                       [:div.center.margin-2rem
                        [:div.center {:style {:font-size "small"}} (rum/react user-key)]]])]
                   [:h3 "Step 2: Click here to start a Facebook chat with " [:a {:href (str "https://facebook.com/" (:seller-fb-id contract)) :target "_blank"} (:seller-name contract)]]
                   [:div.center
                    [:a.hint--bottom {:aria-label (str "Start a Facebook chat with " (:seller-name contract)) :href (str "https://facebook.com/messages/t/" (:seller-fb-id contract)) :target "_blank"}
                     [:img.circle {:src (:seller-photo contract)}]]]
                   [:h3 "Step 3: On Facebook, " [:a {:href "https://www.youtube.com/watch?v=ZeAJDFgcCYA" :target "_blank"} "send a video message to Cedric."] " Record yourself reading this script."]
                   (legal-text :buyer)
                   [:h3 "Step 4: send "
                    [:span {:style {:color "rgb(0, 188, 212)"}}
                     (common/currency->symbol (:currency-buyer contract))
                     [:strong (common/round-currency
                               (* (common/currency-as-float (:amount contract)
                                                            (:currency-seller contract))
                                  (.-rep (:exchange-rate contract)))
                               (:currency-seller contract))]] " to @" (:transfer-info contract) " in Venmo"]]
                  ;; Seller dialog
                  [:div.padding-1rem
                   [:h3 (gstring/format "Step 1: Expect a video from %s reading the following text" (:buyer-name contract))]
                   [:div.center
                    [:a.hint--bottom {:aria-label (:buyer-name contract)}
                     [:img.circle {:src (:buyer-photo contract)}]]]
                   (legal-text :seller)
                   [:h3 "Step 2: As soon as you receive a valid video in Facebook and the funds in Venmo, press the button below:"]
                   [:div.center
                    (ui/raised-button {:label "I've received the funds"
                                       :primary true
                                       :on-touch-tap #(do (actions/mark-contract-transfer-received (:id contract))
                                                          (close-display-contract!))})]])
                buttons
                [(ui/flat-button {:label "Close"
                                  :key (str "wt-close-button-contract-" contract-id)
                                  :primary true
                                  :on-touch-tap close-display-contract!})]]
            (if (rum/react small-display?)
              (mobile-overlay true content [:div {:style {:height "2rem"}}] buttons)
              (ui/dialog {:title (str "Smart Contract ID: " (:human-id contract))
                          :open true
                          :modal true
                          :content-style {:width "500px"}
                          :actions buttons}
                         content)))

          "contract-success"
          (when (am-i-buyer? contract) (escrow-release-dialog :buyer))

          "contract-broken"
          (when (and (am-i-seller? contract) (:escrow-funded-timestamp contract))
            (escrow-release-dialog :seller))

          "contract-broken/escrow-insufficient"
          (when (am-i-seller? contract) (escrow-release-dialog :seller))

          [:div "Unknown contract state. Please contact us."])))))

(rum/defc contract-listing-comp
  < rum/reactive
  []
  (let [_small-display? (rum/react small-display?)
        contracts (rum/react (:contracts state/app))
        server-time (rum/react (:server-time state/app))]
    (cond
      (nil? contracts)
      [:div "Retrieving contracts..."
       (ui/linear-progress {:size 60 :mode "indeterminate"})]
      (not-empty contracts)
      [:div
       [:h4.center.hint--bottom.hint--large {:aria-label "Smart contracts are computer protocols that facilitate, verify, or enforce the negotiation or performance of a contract, or that make a contractual clause unnecessary. Smart contracts often emulate the logic of contractual clauses." :style {:width "100%"}} "Smart Contracts"]
       [:div
        (contract-dialog)
        (ui/list
         (for [contract contracts]
           (ui/list-item
            {:key (str "contract-list-item-" (:id contract))
             :primary-toggles-nested-list true
             :nested-items [(ui/list-item
                             {:key (str "contract-list-item-nested-" (:id contract)) :disabled true}
                             [:div
                              (ui/stepper ((if _small-display? #(assoc % :connector nil) identity)
                                           {:active-step (case (:stage contract)
                                                           "waiting-start" 0
                                                           "waiting-escrow" 1
                                                           "waiting-transfer" 2
                                                           ("contract-success" "contract-broken" "contract-broken/escrow-insufficient") 3
                                                           3)
                                            :orientation (if _small-display? "vertical" "horizontal")
                                            :style (if _small-display? {:width "12rem" :margin "0 auto"} {})})
                                          (ui/step (ui/step-label "Contract initialization"))
                                          (ui/step (ui/step-label "Fund Smart Contract"))
                                          (ui/step (ui/step-label "Contract execution")))
                              [:p.center [:strong "Smart Contract ID: "] (:human-id contract)]
                              (let [explorer-url (case globals/*env*
                                                   "production" "https://blockchain.info/tx/"
                                                   "staging" "https://tbtc.blockr.io/tx/info/"
                                                   "")]
                                [:div.center
                                 [(if (:input-tx contract) :a.tx-link :a.tx-link-shadow) (when (:input-tx contract) {:href (str explorer-url (:input-tx contract)) :target "_blank"})
                                  [:strong "Input transaction"]]
                                 [:span.tx-link " / "]
                                 [(if (:output-tx contract) :a.tx-link :a.tx-link-shadow) (when (:output-tx contract) {:href (str explorer-url (:output-tx contract)) :target "_blank"})
                                  [:strong "Output transaction"]]])
                              (when (case (:stage contract)
                                      ("waiting-start" "waiting-transfer") true
                                      "waiting-escrow" (am-i-seller? contract)
                                      false)
                                [:div.center {:style {:margin-bottom "5rem"}}
                                 (ui/flat-button {:key (str "cancel-button-contract-" (:id contract))
                                                  :label "Break contract"
                                                  :style {:margin "0 1rem 0 1rem"}
                                                  :on-touch-tap #(when (js/confirm "Are you sure?") (actions/break-contract (:id contract)))})])])]}
            [:div.hint--bottom {:aria-label (case (:stage contract)
                                              "waiting-start" (if (am-i-seller? contract)
                                                                (gstring/format "You must coordinate a start-time for this trade with %s on Facebook Messenger. Once started you’ll have 60 minutes to fund the Smart Contract." (:buyer-name contract))
                                                                (gstring/format "Waiting for %s and you to agree on initiation time." (:seller-name contract)))
                                              "waiting-escrow" (if (am-i-seller? contract)
                                                                 "Waiting for you to deposit Bitcoins into this Smart Contract"
                                                                 (gstring/format "Waiting for %s to deposit Bitcoins into this Smart Contract" (:seller-name contract)))
                                              "")
                                :style {:width "100%"}}
             [(if _small-display? :div.center :div.column-half)
              [:div {:style (when-not _small-display? {:display "block" :clear "both"})}
               [:img.circle {:style (if _small-display?
                                      {:margin-bottom "10px"}
                                      {:float "left" :width "28px" :height "28px" :margin-right "4px"})
                             :src (if (am-i-seller? contract) (:buyer-photo contract) (:seller-photo contract))}]]
              [:strong (if (am-i-seller? contract) "SELLER" "BUYER")]
              (gstring/format " // %s BTC " (common/satoshi->btc (:amount contract)))
              (when-not _small-display? [:span {:style {:font-size "x-small" :display "table-cell" :vertical-align "middle"}} (str "Smart Contract ID: " (:human-id contract))])]
             (let [action-required (fn [text]
                                     [(if _small-display? :div.center.margin-1rem-top :div.column-half)
                                      [:div.center.action-required {:on-click #(reset! (:display-contract state/app) (:id contract))} text]])
                   status-class (if _small-display? :div.center.margin-1rem-top :div.column-half)
                   waiting [status-class [:div.center "WAITING"]]
                   time-left (contract-time-left contract server-time)]
               (case (:stage contract)
                 "waiting-start" (action-required (gstring/format "ACTION REQUIRED (%s left)" time-left))
                 "waiting-escrow" (if (am-i-seller? contract)
                                    (action-required (gstring/format "ACTION REQUIRED (%s left)" time-left))
                                    [status-class [:div.center (gstring/format "WAITING (%s left)" time-left)]])
                 "waiting-transfer" (cond (and (:transfer-received contract) (am-i-seller? contract))
                                          [status-class [:div.center "RELEASING TO BUYER"]]
                                          (zero? time-left)
                                          [status-class [:div.center "CONTRACT BROKEN"]]
                                          :else
                                          (action-required (gstring/format "ACTION REQUIRED (%s left)" time-left)))
                 "contract-success" (if (am-i-seller? contract)
                                      [status-class [:div.center "RELEASING TO BUYER"]]
                                      (case (:escrow-release contract)
                                        "<fresh>" (action-required "RELEASE BITCOINS")
                                        "<failure>" (action-required "FAILED RELEASE")
                                        "<success>" [status-class [:div.center (str "RELEASED TO: " (:output-address contract))]]
                                        "<processing>" [status-class [:div.center (str "RELEASING TO: " (:output-address contract))]]
                                        [status-class [:div.center "UNKNOWN ESCROW STATE"]]))
                 "contract-broken" (if (am-i-buyer? contract)
                                     (if (:escrow-funded-timestamp contract)
                                       [status-class [:div.center "PERFORM CHARGEBACK"]]
                                       [status-class [:div.center "CONTRACT BROKEN"]])
                                     (if (:escrow-funded-timestamp contract)
                                       (case (:escrow-release contract)
                                         "<fresh>" (action-required "RELEASE BITCOINS")
                                         "<failure>" (action-required "FAILED RELEASE")
                                         "<success>" [status-class [:div.center (str "RELEASED TO: " (:output-address contract))]]
                                         "<processing>" [status-class [:div.center (str "RELEASING TO: " (:output-address contract))]]
                                         [status-class [:div.center "UNKNOWN ESCROW STATE"]])
                                       [status-class [:div.center "CONTRACT BROKEN"]]))
                 "contract-broken/escrow-insufficient" (if (am-i-buyer? contract)
                                                         [status-class [:div.center "SELLER FAILED TO FUND"]]
                                                         (case (:escrow-release contract)
                                                           "<fresh>" (action-required "INSUFFICIENT FUNDS")
                                                           "<failure>" (action-required "FAILED RELEASE")
                                                           "<success>" [status-class [:div.center (str "RELEASED TO: " (:output-address contract))]]
                                                           "<processing>" [status-class [:div.center (str "RELEASING TO: " (:output-address contract))]]
                                                           [status-class [:div.center "UNKNOWN ESCROW STATE"]]))
                 [status-class [:div.center "UNDEFINED"]]))
             [:div {:style {:clear "both"}}]])))]])))

(rum/defc generic-notifications
  < rum/reactive
  []
  (let [notifications (rum/react (:notifications state/app))
        current (peek notifications)]
    (ui/snackbar {:open (boolean (not-empty notifications))
                  :message (str (:title current) " " (:message current))
                  :on-request-close #(swap! (:notifications state/app) pop)
                  ;; :actions [(ui/flat-button {:label "OK"
                  ;;                            :primary true
                  ;;                            :on-touch-tap (or (:on-touch-tap current)
                  ;;                                              (fn [] (chsk-send!
                  ;;                                                      [:notification/ack {:user-hash @(:user-hash state/app)
                  ;;                                                                          :uuid (:uuid current)}]
                  ;;                                                      5000
                  ;;                                                      #(when (sente/cb-success? %)
                  ;;                                                         (swap! (:notifications state/app) pop)))))})]
                  })))

(rum/defcs sell-offer-matched-dialog
  < rum/reactive
  (rum/local {:name ""} ::account-info)
  [state_]
  (let [pending-matches (rum/react (:sell-offer-matches state/app))
        open? (boolean (not-empty pending-matches))
        current (peek pending-matches)
        account-info (::account-info state_)]
    (when current
      (let [buttons [(ui/flat-button {:label "Accept"
                                      :key (str "br-accept-" (:id current))
                                      :primary true
                                      :disabled (clojure.string/blank? (:name (rum/react account-info)))
                                      :on-touch-tap (fn []
                                                      (swap! (:sell-offer-matches state/app) pop)
                                                      (actions/accept-buy-request
                                                       (:id current)
                                                       ;;(clojure.string/join "\n" (map (fn [[k v]] (gstring/format "%s: %s" (name k) v)) @account-info))
                                                       (:name @account-info)))})
                     (ui/flat-button {:label "Decline"
                                      :on-touch-tap (fn []
                                                      (actions/decline-buy-request (:id current))
                                                      (actions/close-sell-offer #(swap! (:sell-offer-matches state/app) pop)))})]
            ;; Here we calculate the amount the seller is going to put in the escrow
            title "We found a buyer!"
            content [:div
                     [:h3 (gstring/format "%s wants to buy %f Bitcoins"
                                          (:buyer-name current)
                                          (common/round-currency
                                           (* (common/currency-as-float (:amount current) (:currency-seller current))
                                              (common/long->decr (:premium current)))
                                           :btc))]
                     [:div.center {:style {:margin-top "-1rem" :margin-bottom "1rem"}}
                      [:a.hint--bottom {:aria-label (:buyer-name current)}
                       [:img.circle {:src (:buyer-photo current)}]]]
                     [:div.left [:span {:style {:margin-right "2px"}} "@"]
                      (ui/text-field {:id "account-name"
                                      :hint-text "Enter your Venmo ID to get paid!"
                                      ;; :floating-label-text "Venmo ID"
                                      :value (:name (rum/react account-info))
                                      :on-change #(swap! account-info assoc :name (.. % -target -value))})]]]
        (if (rum/react small-display?)
          (mobile-overlay
           open?
           (into [:div.padding-1rem
                  [:h3 title]
                  [:div content]]
                 buttons))
          (ui/dialog {:title title
                      :open open?
                      :modal true
                      :actions buttons
                      :content-style {:width "340px"}}
                     [:div.center content]))))))

(rum/defc footer
  < rum/reactive
  []
  [:div.footer
   [:p.logout (gstring/format "Logged in as %s / " (rum/react (:user-name state/app)))
    [:span {:style {:cursor "pointer"} :on-click actions/logout} "Logout"]]
   [:p.year (gstring/format "Cointrust © %d" (.getFullYear (js/Date.)))]])

(rum/defc facebook-button-comp
  []
  [:div {:style {:width "10rem"}}
   [:img {:style {:float "left" :width "24px" :height "24px" :margin "5px -25px 0 6px" :z-index "-1"}
          :src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjwhLS0gR2VuZXJhdG9yOiBBZG9iZSBJbGx1c3RyYXRvciAxNi4wLjAsIFNWRyBFeHBvcnQgUGx1Zy1JbiAuIFNWRyBWZXJzaW9uOiA2LjAwIEJ1aWxkIDApICAtLT4KCjxzdmcKICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIgogICB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiCiAgIHhtbG5zOnN2Zz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciCiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIKICAgeG1sbnM6c29kaXBvZGk9Imh0dHA6Ly9zb2RpcG9kaS5zb3VyY2Vmb3JnZS5uZXQvRFREL3NvZGlwb2RpLTAuZHRkIgogICB4bWxuczppbmtzY2FwZT0iaHR0cDovL3d3dy5pbmtzY2FwZS5vcmcvbmFtZXNwYWNlcy9pbmtzY2FwZSIKICAgdmVyc2lvbj0iMS4xIgogICBpZD0iTGF5ZXJfMSIKICAgeD0iMHB4IgogICB5PSIwcHgiCiAgIHdpZHRoPSIyNjYuODkzcHgiCiAgIGhlaWdodD0iMjY2Ljg5NXB4IgogICB2aWV3Qm94PSIwIDAgMjY2Ljg5MyAyNjYuODk1IgogICBlbmFibGUtYmFja2dyb3VuZD0ibmV3IDAgMCAyNjYuODkzIDI2Ni44OTUiCiAgIHhtbDpzcGFjZT0icHJlc2VydmUiCiAgIGlua3NjYXBlOnZlcnNpb249IjAuOTEgcjEzNzI1IgogICBzb2RpcG9kaTpkb2NuYW1lPSJmYmxvZ28uc3ZnIj48bWV0YWRhdGEKICAgICBpZD0ibWV0YWRhdGE0MjA5Ij48cmRmOlJERj48Y2M6V29yawogICAgICAgICByZGY6YWJvdXQ9IiI+PGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+PGRjOnR5cGUKICAgICAgICAgICByZGY6cmVzb3VyY2U9Imh0dHA6Ly9wdXJsLm9yZy9kYy9kY21pdHlwZS9TdGlsbEltYWdlIiAvPjwvY2M6V29yaz48L3JkZjpSREY+PC9tZXRhZGF0YT48ZGVmcwogICAgIGlkPSJkZWZzNDIwNyIgLz48c29kaXBvZGk6bmFtZWR2aWV3CiAgICAgcGFnZWNvbG9yPSIjZmZmZmZmIgogICAgIGJvcmRlcmNvbG9yPSIjNjY2NjY2IgogICAgIGJvcmRlcm9wYWNpdHk9IjEiCiAgICAgb2JqZWN0dG9sZXJhbmNlPSIxMCIKICAgICBncmlkdG9sZXJhbmNlPSIxMCIKICAgICBndWlkZXRvbGVyYW5jZT0iMTAiCiAgICAgaW5rc2NhcGU6cGFnZW9wYWNpdHk9IjAiCiAgICAgaW5rc2NhcGU6cGFnZXNoYWRvdz0iMiIKICAgICBpbmtzY2FwZTp3aW5kb3ctd2lkdGg9IjE5ODYiCiAgICAgaW5rc2NhcGU6d2luZG93LWhlaWdodD0iMTIyNyIKICAgICBpZD0ibmFtZWR2aWV3NDIwNSIKICAgICBzaG93Z3JpZD0iZmFsc2UiCiAgICAgaW5rc2NhcGU6em9vbT0iMS45OTMzMDgxIgogICAgIGlua3NjYXBlOmN4PSIxMzMuNDQ2NSIKICAgICBpbmtzY2FwZTpjeT0iMTMzLjQ0NzQ5IgogICAgIGlua3NjYXBlOndpbmRvdy14PSI2NjAiCiAgICAgaW5rc2NhcGU6d2luZG93LXk9IjQ0NSIKICAgICBpbmtzY2FwZTp3aW5kb3ctbWF4aW1pemVkPSIwIgogICAgIGlua3NjYXBlOmN1cnJlbnQtbGF5ZXI9IkxheWVyXzEiIC8+PHBhdGgKICAgICBpZD0iZiIKICAgICBmaWxsPSIjRkZGRkZGIgogICAgIGQ9Ik0xODIuNDA5LDI2Mi4zMDd2LTk5LjgwM2gzMy40OTlsNS4wMTYtMzguODk1aC0zOC41MTVWOTguNzc3YzAtMTEuMjYxLDMuMTI3LTE4LjkzNSwxOS4yNzUtMTguOTM1ICBsMjAuNTk2LTAuMDA5VjQ1LjA0NWMtMy41NjItMC40NzQtMTUuNzg4LTEuNTMzLTMwLjAxMi0xLjUzM2MtMjkuNjk1LDAtNTAuMDI1LDE4LjEyNi01MC4wMjUsNTEuNDEzdjI4LjY4NGgtMzMuNTg1djM4Ljg5NWgzMy41ODUgIHY5OS44MDNIMTgyLjQwOXoiIC8+PC9zdmc+"}]
   [:span.hint--bottom.hint--large {:aria-label "Connect with Facebook friends who are buying / selling Bitcoin. WE PROMISE TO NEVER SPAM you or your friends."}
    [:h1 {:style {:margin "0 0 0 0" :font-size "small" :color "white" :font-weight "lighter"}} "FACEBOOK LOGIN"]]])

(rum/defcs login-comp
  < rum/reactive
  (rum/local false ::fb-error)
  [state_]
  (let [fb-error (::fb-error state_)]
    [:div
     [:div.center
      (ui/mui-theme-provider
       {:mui-theme (get-mui-theme {:raised-button {:primary-color "#3b5998"}})}
       (ui/raised-button {:label "Facebook Login"
                          :primary true
                          :style {:margin "1rem"}
                          :icon (facebook-button-comp)
                          :on-touch-tap #(actions/login fb-error)}))]
     [:div.center
      (ui/dialog {:title "Please refresh this web page"
                  :open (boolean (rum/react fb-error))
                  :on-touch-tap #(reset! fb-error nil)}
                 (str "We had trouble connecting to facebook.  Please refresh your web page.  This will probably work.  If it doesn't please check that you don't have a browser extension that disables the use of Social Logins.  Cointrust uses the social graph to find optimal matches for trading. /// Error /// " (str (rum/react fb-error))))
      [:div.fb-login-button {:data-max-rows "2" :data-size "large" :data-show-faces "true"
                             :data-auto-logout-link "false"}]]
     [:div.frontpage
      (ui/list
       (ui/list-item
        {:key "qa-text-1"
         :style {:padding "0 0 0 0"}
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-1-nested" :disabled true}
                         [:ul
                          [:li "Cointrust makes it easy to buy & sell Bitcoin instantly and risk- free by using Facebook, Venmo, and "
                           [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts"]]])]}
        [:h3 "What is Cointrust?"])
       (ui/list-item
        {:key "qa-text-2"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-2-nested" :disabled true}
                         [:ul
                          [:li "Match - Cointrust will match you with the best sell offer in your friend network."]
                          [:li "Talk - Coordinate the sale with the buyer/seller using Facebook Messenger."]
                          [:li "Pay - Pay via Venmo."]
                          [:li "Settle - You’re protected via " [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts."]
                           " Once both parties have completed the steps all funds are released to both parties and  As long as you follow our instructions you and your money / bitcoin are protected."]])]}
        [:h3 "How does this work?"])
       (ui/list-item
        {:key "qa-text-3"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-3-nested" :disabled true}
                         [:ul
                          [:li "Cointrust makes Bitcoin purchases risk-free for both buyers and sellers through the use of " [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts."]]
                          [:li "Cointrust acts as a notary, confirming both buyer and seller have lived up to their part of the agreement. And protecting the other person if they don’t."]])]}
        [:h3 "Risk Free"])
       (ui/list-item
        {:key "qa-text-4"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-4-nested" :disabled true}
                         [:ul
                          [:li "By using " [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts"] ", Cointrust acts like a digital notary confirming all the information you need as a buyer or seller to be protected and either perform or reverse a chargeback."]])]}
        [:h3 "What about chargebacks?"])
       (ui/list-item
        {:key "qa-text-5"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-5-nested" :disabled true}
                         [:ul
                          [:li "Bitcoin buyers are guaranteed to get what they pay for."]
                          [:li "We hold bitcoins in a " [:span.hint--bottom.hint--large smart-contract-fields "smart contracts"]
                           " (think of it like an escrow account) and confirm the amount the buyer will receive."]])]}
        [:h3 "Am I protected if I’m buying Bitcoin?"])
       (ui/list-item
        {:key "qa-text-6"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-6-nested" :disabled true}
                         [:ul
                          [:li "Bitcoin sellers are guaranteed to get paid for what they sold."]
                          [:li "Bitcoin funds are held in escrow."]
                          [:li "Buyers provide a video contract which protects sellers from chargebacks. And allows them to reverse chargebacks if they act honestly."]])]}
        [:h3 "Am I protected if I’m selling Bitcoin?"]))]]))

(rum/defc main-comp
  []
  [:div
   (menu-controls-comp)
   (sell-offer-comp)
   (ui/divider)
   (request-listing-comp)
   (ui/divider)
   (contract-listing-comp)
   (buy-dialog)
   (sell-dialog)
   (generic-notifications)
   (sell-offer-matched-dialog)
   (footer)
   (top-notification-bar)])

(rum/defc app
  < rum/reactive
  []
  (let [user (rum/react (:user-hash state/app))]
    [:div {:style {:position "absolute"
                   :max-width "700px"
                   :margin "auto" :top "2rem" :bottom "0" :left "0" :right "0"
                   :padding "1rem"}}
     (ui/mui-theme-provider
      {:mui-theme (get-mui-theme {:palette {:text-color (color :grey900)}})}
      [:div
       [:h1.title.center "COINTRUST"]
       (when-not user [:h3.center "Bitcoin buy/sell with your friend network"])
       (if user
         (main-comp)
         (login-comp))])]))
