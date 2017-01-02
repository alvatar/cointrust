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
            [cljs-hash.goog :as gh]))


;;
;; Globals
;;

(goog-define is-dev?_ false)
(def hook-fake-id?_ true)

(defonce app-error (atom nil))
(defonce app-state
  (atom {:scene "main-menu"
         :user-fbid nil
         :user-hash nil
         :user-id nil
         :friend-fbids []
         :friend-hashes []
         :friends2 []
         :btc-usd 769.5
         :sell-offer {:min 200 :max 20000}
         :offer-match nil
         :contracts nil}))

(def db-schema {})
(def db-conn (d/create-conn db-schema))

;;
;; Utils
;;

(defn clj->json [ds] (.stringify js/JSON (clj->js ds)))

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
  (js/console.log "Initializing Sente...")
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
      (js/console.log "Initializing Sente client router")
      (reset! router_
              (sente/start-client-chsk-router!
               ch-chsk event-msg-handler)))
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

(defn logout [] (js/console.log "LOGOUT TODO"))

(defn get-user-contracts []
  (chsk-send!
   [:user/contracts {:user-id (:user-id @app-state)}] 10000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (swap! oracle.core/app-state assoc :contracts (:contracts resp))
       (do (reset! app-error "There was an error retrieving your previous contracts. Please try again.")
           (js/console.log "Error in get-user-contract: " (str resp))))
     (js/console.log "Contracts: " (str (:contracts @app-state))))))

(defn initiate-contract [btc-amount callback]
  (chsk-send!
   [:contract/initiate {:user-id (:user-id @app-state)
                        :btc-amount btc-amount}] 20000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (swap! oracle.core/app-state update :contracts #(conj % (:contract resp)))
       (do (reset! app-error "There was an error with your contract. Please try again.")
           (js/console.log "Error in initiate-contract: " (str resp))))
     (callback)
     (js/console.log "Contracts: " (str (:contracts @app-state))))))

(defn handle-enter [user-id]
  (swap! app-state assoc :user-id user-id)
  (get-user-contracts)
  (chsk-send!
   [:user/friends-of-friends {:user-id user-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (swap! app-state assoc :friends2 (:friends2 resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (js/console.log "Error in handle-enter: " (str resp))))
     (js/console.log "Friends^2" (str (:friends2 @app-state))))))

(defn try-enter [hashed-id hashed-friends]
  (chsk-send!
   [:user/enter {:hashed-user hashed-id
                 :hashed-friends hashed-friends}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (handle-enter (:found-user resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (js/console.log "Error in try-enter: " (str resp)))))))

(defn set-facebook-ids []
  (fb/get-login-status
   (fn [response]
     (if (= (:status response) "connected")
       (let [user-fbid (get-in response [:authResponse :userID])
             hashed-id (cljs-hash.goog/hash :sha1 (str user-fbid))
             friend-fbids (fb/api "/me/friends" {} identity)
             hashed-friends (mapv #(cljs-hash.goog/hash :sha1 (str %)) friend-fbids)]
         (swap! app-state assoc :user-fbid user-fbid)
         (swap! app-state assoc :user-hash hashed-id)
         (swap! app-state assoc :friend-fbids friend-fbids)
         (swap! app-state assoc :friend-hashes hashed-friends)
         (js/console.log "Connected with Facebook userID: " user-fbid)
         (js/console.log "Hashed user: " hashed-id)
         (js/console.log "Friend IDs: " (str friend-fbids))
         (js/console.log "Hashed friends: " (str hashed-friends))
         (sente-register-init-callback! #(try-enter hashed-id hashed-friends))
         (init-sente! hashed-id))
       (js/console.log "Not logged in: " (clj->js response))))))

;;
;; Event Handlers
;;

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event]}]
  (js/console.log "Unhandled event: " event))

;; TODO: You'll want to listen on the receive channel for a [:chsk/state [_ {:first-open? true}]] event. That's the signal that the socket's been established.
(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (let [[old-state-map new-state-map] (have vector? ?data)]
    (if (:first-open? new-state-map)
      (doseq [cb @sente-callback-registry_] (cb))
      (js/console.log "Channel socket state change: " new-state-map))))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (js/console.log "Push event from server: " ?data))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (when is-dev?_ (js/console.log "Handshake"))))

(defmethod -event-msg-handler :contract/update
  [{:as ev-msg :keys [?data]}]
  (let [{:keys [id stage status]} ?data]
    (swap! app-state assoc :contracts
           (for [c (:contracts @app-state)]
             (if (= (:id c) id) (merge c {:stage stage :status status}) c)))))

(defmethod -event-msg-handler :offer/matched
  [{:as ev-msg :keys [?data]}]
  (let [{:keys [id stage status]} ?data]
    ()))

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
                               (js/console.log "Connected with fake user hash: " hashed-id)
                               (swap! app-state assoc :user-hash hashed-id)
                               (sente-register-init-callback! #(try-enter hashed-id ["TODO"]))
                               (init-sente! hashed-id))
                             (fb/get-login-status
                              (fn [response]
                                (case (:status response)
                                  "connected"
                                  (js/console.log "Connected with userID: " (get-in response [:authResponse :userID]))
                                  (fb/login set-facebook-ids {:scope "public_profile,email,user_friends"}))))))})]])))

(rum/defcs buy-dialog
  < rum/reactive (rum/local {:btc-amount 1.0} ::input)
  [state parent-component-mode]
  (let [input (::input state)]
    (ui/dialog {:title "Buy Bitcoins"
                :actions [(ui/flat-button {:label "Buy"
                                           :primary true
                                           :disabled (:processing (rum/react input))
                                           :on-touch-tap
                                           (fn [e]
                                             (swap! input assoc :processing true)
                                             (initiate-contract (:btc-amount @input)
                                                                #(do (reset! parent-component-mode :none)
                                                                     (swap! input assoc :processing false))))})
                          (ui/flat-button {:label "Cancel"
                                           :on-touch-tap #(reset! parent-component-mode :none)})]
                :open (= @parent-component-mode :buy)
                :modal true}
               [:div
                (let [btc-usd (:btc-usd app-state)
                      total (* btc-usd (:btc-amount (rum/react input)))]
                  [:div [:h4 "Bitcoin price: " btc-usd " BTC/USD (Coinbase reference rate)"]
                   (ui/text-field {:id "btc-amount"
                                   :autoFocus true
                                   :value (:btc-amount (rum/react input))
                                   :on-change (fn [e] (swap! input assoc :btc-amount (.. e -target -value)))
                                   :errorText (when (<= total 0) "Invalid value")})
                   (when (> total 0)
                     (str "for " (/ (long (* 100000 total)) 100000) " USD"))])
                (when (:processing (rum/react input))
                  [:div
                   (ui/linear-progress {:size 60 :mode "indeterminate"
                                        :style {:margin "auto" :left "0" :right "0" :top "1.5rem"}})
                   [:h5 {:style {:text-align "center" :margin-top "2rem"}} "Initiating contract"]])])))

(rum/defcs sell-dialog
  < (rum/local {} ::ui-values)
  [state parent-component-mode]
  (let [ui-values (::ui-values state)
        min-val (or (:min @ui-values) (:min (:sell-offer @app-state)) 200)
        max-val (or (:max @ui-values) (:max (:sell-offer @app-state)) 20000)]
    (ui/dialog {:title "Sell Bitcoins"
                :actions [(ui/flat-button {:label "Sell"
                                           :primary true
                                           :on-touch-tap (fn [] (swap! app-state assoc :sell-offer {:min min-val :max max-val})
                                                           (reset! parent-component-mode :none))})
                          (ui/flat-button {:label "Cancel"
                                           :on-touch-tap #(reset! parent-component-mode :none)})]
                :open (= @parent-component-mode :sell)
                :modal true}
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
  [component-mode]
  [:div
   [:h1 {:style {:text-align "center"}} "Cointrust"]
   [:h3 {:style {:text-align "center"}} "Friend of Friend Bitcoin Trading"]
   [:h5 {:style {:text-align "center"}} (str "You can trade with " (:friends2 (rum/react app-state)) " partners")]
   (when-let [error (rum/react app-error)]
     [:h5 {:style {:text-align "center" :color "#f00"}} error])
   [:div {:style {:text-align "center"}}
    ;; TODO: hints http://kushagragour.in/lab/hint/
    (ui/raised-button {:label "I want to BUY Bitcoins"
                       :disabled true ;;(not (or (= (:contracts app-state) :unknown) (empty? (:contracts app-state))))
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! component-mode :buy)})
    (ui/raised-button {:label (if (:sell-offer (rum/react app-state)) "Change sell offer" "I want to SELL Bitcoins")
                       :disabled false
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! component-mode :sell)})]])

(rum/defc offer-progress-comp
  < rum/reactive
  []
  (when (:sell-offer (rum/react app-state))
    [:div
     [:h4 {:style {:text-align "center"}} "My selling offer"]
     [:p "You are currently offering to sell: "
      [:strong (:min (:sell-offer @app-state))]
      " (min.) - "
      [:strong (:max (:sell-offer @app-state))]
      " BTC (max.)"]
     [:div (ui/stepper {:active-step 0}
                       (ui/step (ui/step-label "Make Offer"))
                       (ui/step (ui/step-label "Wait for Match"))
                       (ui/step (ui/step-label "Initiate Contract")))]]))

(rum/defc offer-matched-dialog
  []
  (ui/dialog {:titled "Offer Matched"
              :open (boolean (:offer-match @app-state))
              :modal true}
             "HELLO"))

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
    (cond
      (not (:contracts @app-state))
      [:div "Retrieving contracts..."
       (ui/linear-progress {:size 60 :mode "indeterminate"})]
      (empty? (:contracts @app-state))
      "No contracts in history"
      :else
      (for [contract (:contracts @app-state)]
        [:div (str "Contract Hash ID: " (:hash contract))
         (map-indexed
          (fn [ix text] (contract-stage-comp contract ix text))
          ["Stage 1" "Stage 2" "Stage 3" "Stage 4"])]))]])

(rum/defcs main-comp
  < (rum/local :none ::mode)
  [state]
  (let [component-mode (::mode state)]
    (ui/mui-theme-provider
     {:mui-theme (get-mui-theme {:palette {:text-color (color :blue900)}})}
     (ui/paper
      [:div
       (menu-controls-comp component-mode)
       (offer-progress-comp)
       (contract-listing-comp)
       (buy-dialog component-mode)
       (sell-dialog component-mode)
       (offer-matched-dialog)]))))

(rum/defc faq
  []
  (ui/mui-theme-provider
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
        (let [state (rum/react app-state)]
          (if (:user-hash state)
            (case (:scene state)
              "main-menu"
              [(main-comp) (faq)])
            [(login-comp) (faq)]))))

(rum/mount (app) (js/document.getElementById "app"))

;; (def ui-values (atom {:val 500}))

;; (rum/defc mycomp < rum/reactive []
;;   (ui/mui-theme-provider
;;    {:mui-theme (get-mui-theme {:palette {:text-color (color :blue900)}})}
;;    [:div [:p (:val (rum/react ui-values))] ; <-- removing this will avoid the error
;;     (ui/slider {:min 200 :max 20000
;;                 :value (:val (rum/react ui-values))
;;                 :on-change #(swap! ui-values assoc :val %2)})]))

;; (rum/mount (mycomp) (js/document.getElementById "app"))
