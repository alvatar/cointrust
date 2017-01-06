(ns oracle.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs-react-material-ui.core :refer [adapt-rum-class]])
  (:require [cljs.core.async :as async :refer (<! >! put! take! chan)]
            [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            ;; Material UI
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.icons :as ic]
            [cljs-react-material-ui.rum :as ui]
            [cljsjs.react-flexbox-grid]
            ;; React
            [rum.core :as rum]
            ;; Database
            [datascript.core :as d]
            ;; Facebook
            [fb-sdk-cljs.core :as fb]
            ;; Crypto
            [cljs-hash.goog :as gh]
            ;; Format
            [goog.string :as gstring]))


;;
;; Globals
;;

(goog-define is-dev?_ false)
(def hook-fake-id?_ true)

(defonce app-error (atom nil))
(defonce app-state {:scene (atom "main-menu")
                    :ui-mode (atom :none)
                    :user-fbid (atom nil)
                    :user-hash (atom nil)
                    :user-id (atom nil)
                    :friend-fbids (atom [])
                    :friend-hashes (atom [])
                    :friends2 (atom [])
                    :btc-usd (atom 1025.0)
                    :sell-offer (atom nil)
                    :offer-match (atom nil)
                    :contracts (atom nil)})

(def db-schema {})
(def db-conn (d/create-conn db-schema))

;;
;; Utils
;;

(defn clj->json [ds] (.stringify js/JSON (clj->js ds)))

(defn log* [& [args]] (when is-dev?_ (js/console.log (apply str args))))

;; https://github.com/roylee0704/react-flexbox-grid
(def ui-flexbox-grid (adapt-rum-class js/ReactFlexboxGrid.Grid))
(def ui-flexbox-row (adapt-rum-class js/ReactFlexboxGrid.Row))
(def ui-flexbox-col (adapt-rum-class js/ReactFlexboxGrid.Col))

;;
;; Setup
;;


(enable-console-print!)

(defonce router_ (atom nil))
(def sente-callback-registry_ (atom []))

(defn sente-register-init-callback! [callback]
  (swap! sente-callback-registry_ conj callback))

(defn init-sente! [hashed-id]
  (log* "Initializing Sente...")
  (let [packer (sente-transit/get-transit-packer)
        {:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket! "/chsk" { ;; :host (if *is-dev*
                                             ;;         (do (js/console.log "Connecting to localhost")
                                             ;;             "192.168.4.16:5000")
                                             ;;         (do (js/console.log "Connecting to Heroku")
                                             ;;             "ikidz.herokuapp.com"))
                                             ;; :protocol (if *is-dev* "http:" "https:")
                                             :client-id hashed-id
                                             :type :auto
                                             :packer packer})]
    (def chsk chsk)
    (def ch-chsk ch-recv)             ; ChannelSocket's receive channel
    (def chsk-send! send-fn)          ; ChannelSocket's send API fn
    (def chsk-state state)            ; Watchable, read-only atom
    (declare event-msg-handler)
    (defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
    (defn start-router! []
      (stop-router!)
      (log* "Initializing Sente client router")
      (reset! router_ (sente/start-client-chsk-router! ch-chsk event-msg-handler)))
    (start-router!)))

;; For testing
(when-not hook-fake-id?_
  (fb/load-sdk (fn []
                 (println "Facebook lib loaded")
                 (fb/init {:appId "1131377006981108"
                           :status true
                           :cookies true
                           :xfbml true
                           :version "v2.6"}))))

;;
;; Actions
;;

(defn logout [] (log* "LOGOUT TODO"))

(defn get-user-contracts []
  (chsk-send!
   [:user/contracts {:user-id @(:user-id app-state)}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when (:contracts resp) (reset! (:contracts app-state) (:contracts resp)))
       (do (reset! app-error "There was an error retrieving your previous contracts. Please try again.")
           (log* "Error in get-user-contract: " (str resp))))
     (log* "Contracts: " (str @(:contracts app-state))))))

(defn open-sell-offer [{:as vals :keys [min max]}]
  (chsk-send!
   [:offer/open (assoc vals :user-id @(:user-id app-state))] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:sell-offer app-state) (select-keys resp [:min :max]))
       (reset! app-error "There was an error opening the sell offer. Please try again.")))))

(defn get-active-sell-offer []
  (chsk-send!
   [:offer/get {:user-id @(:user-id app-state)}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (let [offer (select-keys resp [:min :max])]
         (when (not-empty offer) (reset! (:sell-offer app-state) offer)))
       (reset! app-error "There was an error retrieving the sell offer.")))))

;; Run when user we change the User ID
(add-watch (:user-id app-state) :got-user-id #(when %4 (get-active-sell-offer)))

(defn close-sell-offer []
  (chsk-send!
   [:offer/close {:user-id @(:user-id app-state)}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:sell-offer app-state) nil)
       (reset! app-error "There was an error closing the sell offer. Please try again.")))))

(defn request-contract [amount callback]
  (chsk-send!
   [:contract/request {:user-id @(:user-id app-state)
                       :amount amount
                       :currency "xbt"}] 20000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       ;; (swap! (:contracts app-state) #(conj % (:contract resp)))
       (do (log* "Contract requested")
           ;; TODO  XXX
           )
       (do (reset! app-error "There was an error with your contract. Please try again.")
           (log* "Error in request-contract: " (str resp))))
     (callback)
     (log* "Contracts: " (str @(:contracts app-state))))))

(defn handle-enter [user-id]
  (reset! (:user-id app-state) user-id)
  (get-user-contracts)
  (chsk-send!
   [:user/friends-of-friends {:user-id user-id}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:friends2 app-state) (:friends2 resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (log* "Error in handle-enter: " (str resp))))
     (log* "Friends^2" (str @(:friends2 app-state))))))

(defn try-enter [hashed-id hashed-friends]
  (chsk-send!
   [:user/enter {:hashed-user hashed-id
                 :hashed-friends hashed-friends}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (handle-enter (:found-user resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (log* "Error in try-enter: " (str resp)))))))

(defn set-facebook-ids []
  (fb/get-login-status
   (fn [response]
     (if (= (:status response) "connected")
       (let [user-fbid (get-in response [:authResponse :userID])
             hashed-id (cljs-hash.goog/hash :sha1 (str user-fbid))
             friend-fbids (fb/api "/me/friends" {} identity)
             hashed-friends (mapv #(cljs-hash.goog/hash :sha1 (str %)) friend-fbids)]
         (reset! (:user-fbid app-state) user-fbid)
         (reset! (:user-hash app-state) hashed-id)
         (reset! (:friend-fbids app-state) friend-fbids)
         (reset! (:friend-hashes app-state) hashed-friends)
         (log* "Connected with Facebook userID: " user-fbid)
         (log* "Hashed user: " hashed-id)
         (log* "Friend IDs: " (str friend-fbids))
         (log* "Hashed friends: " (str hashed-friends))
         (sente-register-init-callback! #(try-enter hashed-id hashed-friends))
         (init-sente! hashed-id))
       (log* "Not logged in: " (clj->js response))))))

;;
;; Event Handlers
;;

;; App-level messages

(defmulti app-msg-handler first)

(defmethod app-msg-handler :default
  [app-msg]
  (log* "Unhandled app event: " (str app-msg)))

(defmethod app-msg-handler :contract/update
  [[_ {:as app-msg :keys [stage status id amount]}]]
  (reset! (:contracts app-state)
          (for [c @(:contracts app-state)]
            (if (= (:id c) id) (merge c {:stage stage :status status}) c))))

;; (chsk-send! "asdf" [:offer/matched {:status :ok :amount 300}])
(defmethod app-msg-handler :offer/matched
  [[_ {:as app-msg :keys [status amount]}]]
  (if (and (= status :ok) amount)
    (reset! (:offer-match app-state) amount)
    (log* "Error in :offer/matched message")))

;; Sente-level messages

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler :default
  [{:as ev-msg :keys [event]}]
  (log* "Unhandled event: " (str event)))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (let [[old-state-map new-state-map] (have vector? ?data)]
    (if (:first-open? new-state-map)
      (doseq [cb @sente-callback-registry_] (cb))
      (log* "Channel socket state change: " new-state-map))))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (when is-dev?_ (log* "Handshake"))))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (log* "Push event from server: " (str ?data))
  (app-msg-handler ?data))

;;
;; UI Components
;;

(rum/defc login-comp
  []
  (ui/mui-theme-provider
   {:mui-theme (get-mui-theme {:palette {:text-color (color :blue900)}})}
   (ui/paper
    [:div
     [:h1 {:style {:text-align "center"}} "Cointrust"]
     [:h3 {:style {:text-align "center"}} "Friend of Friend Bitcoin Trading"]
     [:div {:style {:text-align "center"}}
      (ui/raised-button {:label "Ephemeral Login"
                         :style {:margin "1rem"}
                         :on-touch-tap
                         (fn [e]
                           (if hook-fake-id?_
                             (let [hashed-id "asdf" user-id 1]
                               (log* "Connected with fake user hash: " hashed-id)
                               (reset! (:user-hash app-state) hashed-id)
                               (sente-register-init-callback! #(try-enter hashed-id ["TODO"]))
                               (init-sente! hashed-id))
                             (fb/get-login-status
                              (fn [response]
                                (case (:status response)
                                  "connected"
                                  (log* "Connected with userID: " (get-in response [:authResponse :userID]))
                                  (fb/login set-facebook-ids {:scope "public_profile,email,user_friends"}))))))})]])))

(rum/defcs buy-dialog
  < rum/reactive (rum/local {:amount 1.0} ::input)
  [state]
  (let [input (::input state)
        valid-val (fn [x] (and (number? x) (> x 0)))
        btc-usd @(:btc-usd app-state)
        total (* btc-usd (:amount (rum/react input)))]
    (ui/dialog {:title "Buy Bitcoins"
                :open (= (rum/react (:ui-mode app-state)) :buy-dialog)
                :modal true
                :actions [(ui/flat-button {:label "Buy"
                                           :primary true
                                           :disabled (or (:processing (rum/react input))
                                                         (not (valid-val total)))
                                           :on-touch-tap
                                           (fn [e]
                                             (when (valid-val total)
                                               (swap! input assoc :processing true)
                                               (request-contract (:amount @input)
                                                                 #(do (reset! (:ui-mode app-state) :none)
                                                                      (swap! input assoc :processing false)))))})
                          (ui/flat-button {:label "Cancel"
                                           :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
               [:div
                [:div [:h4 "Bitcoin price: " btc-usd " BTC/USD (Coinbase reference rate)"]
                 (ui/text-field {:id "amount"
                                 :autoFocus true
                                 :value (:amount (rum/react input))
                                 :on-change (fn [e] (swap! input assoc :amount (.. e -target -value)))
                                 :errorText (when (not (valid-val total)) "Invalid value")})
                 (when (> total 0)
                   (str "for " (/ (long (* 100000 total)) 100000) " USD"))]
                (when (:processing (rum/react input))
                  [:div
                   (ui/linear-progress {:size 60 :mode "indeterminate"
                                        :style {:margin "auto" :left "0" :right "0" :top "1.5rem"}})
                   [:h5 {:style {:text-align "center" :margin-top "2rem"}} "Initiating contract"]])])))

(rum/defcs sell-dialog
  < rum/reactive (rum/local {} ::ui-values)
  [state_]
  (let [offer-active? (boolean @(:sell-offer app-state))
        ui-values (::ui-values state_)
        min-val (or (:min @ui-values) (:min @(:sell-offer app-state)) 200)
        max-val (or (:max @ui-values) (:max @(:sell-offer app-state)) 20000)]
    (ui/dialog {:title (if offer-active? "Active offer" "Sell Bitcoins")
                :open (= (rum/react (:ui-mode app-state)) :sell-dialog)
                :modal true
                :actions [(when offer-active?
                            (ui/flat-button {:label "Remove"
                                             :on-touch-tap (fn []
                                                             (when (js/confirm "Are you sure?")
                                                              (close-sell-offer)
                                                              (reset! (:ui-mode app-state) :none)))}))
                          (ui/flat-button {:label (if offer-active? "Update" "Sell")
                                           :on-touch-tap (fn []
                                                           (open-sell-offer {:min min-val :max max-val})
                                                           (reset! (:ui-mode app-state) :none))})
                          (ui/flat-button {:label "Back"
                                           :primary true
                                           :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
               [:div
                [:p "Minimum amount of Bitcoins I'm willing to sell: " [:strong min-val]]
                (ui/slider {:min 200 :max 20000
                            :value min-val
                            :on-change #(swap! ui-values assoc :min %2)})
                [:p "Maximum amount of Bitcoins I'm willing to sell "  [:strong max-val]]
                (ui/slider {:min 200 :max 20000
                            :value max-val
                            :on-change #(swap! ui-values assoc :max %2)})])))

(rum/defc menu-controls-comp
  < rum/reactive
  []
  [:div
   [:h1 {:style {:text-align "center"}} "Cointrust"]
   [:h3 {:style {:text-align "center"}} "Friend of Friend Bitcoin Trading"]
   [:h5 {:style {:text-align "center"}} (str "You can trade with " (rum/react (:friends2 app-state)) " partners")]
   (when-let [error (rum/react app-error)]
     [:h5 {:style {:text-align "center" :color "#f00"}} error])
   [:div {:style {:text-align "center"}}
    ;; TODO: hints http://kushagragour.in/lab/hint/
    (ui/raised-button {:label "I want to BUY Bitcoins"
                       :disabled false #_(not (let [contracts (rum/react (:contracts app-state))]
                                        (or (= contracts :unknown) (empty? contracts))))
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode app-state) :buy-dialog)})
    (ui/raised-button {:label (if (rum/react (:sell-offer app-state)) "Change sell offer" "I want to SELL Bitcoins")
                       :disabled false
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode app-state) :sell-dialog)})]])

(rum/defc offer-progress-comp
  < rum/reactive
  []
  (when-let [sell-offer (rum/react (:sell-offer app-state))]
   [:div
    [:h4 {:style {:text-align "center"}} "My selling offer"]
    [:p "You are currently offering to sell: "
     [:strong (:min sell-offer)] " (min.) - " [:strong (:max sell-offer)] " BTC (max.)"]
    [:div (ui/stepper {:active-step 1}
                      (ui/step (ui/step-label "Make Offer"))
                      (ui/step (ui/step-label "Wait for Match"))
                      (ui/step (ui/step-label "Initiate Contract")))]]))

(rum/defcs offer-matched-dialog
  < rum/reactive (rum/local true ::decline-lock)
  [state_]
  (let [decline-lock (::decline-lock state_)]
   (ui/dialog {:title "Offer Matched"
               :open (boolean (rum/react (:offer-match app-state)))
               :modal true
               :actions [(ui/flat-button {:label "Accept"
                                          :primary true
                                          :on-touch-tap #()})
                         (ui/flat-button {:label "Decline"
                                          :disabled (rum/react decline-lock)
                                          :on-touch-tap #(reset! (:offer-match app-state) nil)})]}
              [:div
               [:p (gstring/format "A buyer wants to purchase %f BTC (%f USD)." 2.2 2000)]
               [:h6 "You can decline it, but your sell offer will be removed."
                [:br] (ui/checkbox {:label "I understand"
                                    :checked (not (rum/react decline-lock))
                                    :on-check #(reset! decline-lock (not %2))})]])))

(rum/defc contract-stage-comp
  < {:key-fn (fn [_ ix _] (str "stage-display-" ix))}
  [contract ix text]
  (let [stage-color
        (fn [contract stage]
          (let [current-stage (:stage contract)]
            (cond
              (< current-stage stage) "#aaa"
              (= current-stage stage) (case (:status contract)
                                        "waiting" "#0fb"
                                        "done" "#0f0"
                                        "action-required" "#00f"
                                        "error" "#f00")
              (> current-stage stage) "#0f0")))]
    [:div {:style {:background-color (stage-color contract ix)
                   :width "100%" :height "2rem" :margin-top "0.5rem"}}
     text]))

(rum/defc contract-listing-comp
  < rum/reactive
  []
  [:div
   [:h4 {:style {:text-align "center"}} "Active contracts"]
   [:div
    (let [contracts (rum/react (:contracts app-state))]
      (cond
        (not contracts)
        [:div "Retrieving contracts..."
         (ui/linear-progress {:size 60 :mode "indeterminate"})]
        (empty? contracts)
        "No contracts in history"
        :else
        (for [contract contracts]
          [:div {:key (:hash contract)} (str "Contract Hash ID: " (:hash contract))
           (map-indexed
            (fn [ix text] (contract-stage-comp contract ix text))
            ["Stage 1" "Stage 2" "Stage 3" "Stage 4"])])))]])

(rum/defc main-comp
  []
  (ui/mui-theme-provider
   {:mui-theme (get-mui-theme {:palette {:text-color (color :blue900)}})}
   (ui/paper
    [:div
     (menu-controls-comp)
     (offer-progress-comp)
     (contract-listing-comp)
     (buy-dialog)
     (sell-dialog)
     (offer-matched-dialog)])))

(rum/defc faq
  []
  #_(ui/mui-theme-provider
   {:mui-theme (get-mui-theme {:palette {:text-color (color :blue900)}})}
   (ui/paper
    {:style {:margin-top "50px"}}
    [:div
     [:div "How it works..."]
     [:div "FAQ..."]])))

(rum/defc app
  < rum/reactive
  []
  (into [:div {:style {:position "absolute"
                       :max-width "700px" ; :height "500px"
                       :margin "auto" :top "5rem" :bottom "0" :left "0" :right "0"}}]
        (if (rum/react (:user-hash app-state))
          (case @(:scene app-state)
            "main-menu"
            [(main-comp) (faq)])
          [(login-comp) (faq)])))

(rum/mount (app) (js/document.getElementById "app"))
