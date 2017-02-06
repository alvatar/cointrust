(ns oracle.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs-react-material-ui.core :refer [adapt-rum-class]])
  (:require [cljs.core.async :as async :refer (<! >! put! take! chan)]
            [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.icons :as ic]
            [cljs-react-material-ui.rum :as ui]
            [rum.core :as rum]
            [datascript.core :as d]
            [fb-sdk-cljs.core :as fb]
            [cljs-hash.goog :as gh]
            [goog.string :as gstring]
            cljsjs.rc-slider
            ;; -----
            [oracle.common :as common]))


;;
;; Globals
;;

(goog-define *is-dev* false)
(def hook-fake-id?_ false)

(defonce app-error (atom nil))
(defonce app-state {:scene (atom "main-menu")
                    :ui-mode (atom :none)
                    :user-fbid (atom nil)
                    :user-hash (atom nil)
                    :user-id (atom nil)
                    :friend-fbids (atom [])
                    :friend-hashes (atom [])
                    :friends2 (atom [])
                    :exchange-rates (atom {:xbt-usd 927.360})
                    :sell-offer (atom nil)
                    :sell-offer-matches (atom cljs.core/PersistentQueue.EMPTY)
                    :buy-requests (atom [])
                    :contracts (atom nil)
                    :notifications (atom cljs.core/PersistentQueue.EMPTY)
                    :display-contract (atom nil)})

(def db-schema {})
(def db-conn (d/create-conn db-schema))

;;
;; Utils
;;

(defn clj->json [ds] (.stringify js/JSON (clj->js ds)))

(defn log* [& args] (when true #_*is-dev* (js/console.log (clojure.string/join " " (map str args)))))

(defn find-in [col id] (first (keep-indexed #(when (= (:id %2) id) %1) col)))

(defn round-currency [val] (/ (long (* 100000 val)) 100000))

(defn am-i-buyer? [contract] (= @(:user-id app-state) (:buyer-id contract)))
(defn am-i-seller? [contract] (= @(:user-id app-state) (:seller-id contract)))
(defn waiting-transfer? [contract] (= (:stage contract) "waiting-transfer"))

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
        (sente/make-channel-socket! "/chsk" {:client-id hashed-id
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
      (log* "Initializing Sente client router...")
      (reset! router_ (sente/start-client-chsk-router! ch-chsk event-msg-handler)))
    (start-router!)))

;; For testing
(when-not hook-fake-id?_
  (fb/load-sdk (fn []
                 (js/console.log "Facebook lib loaded")
                 (fb/init {:appId "1131377006981108"
                           :status true
                           :cookies false
                           :xfbml true
                           :version "v2.8"}))))

;;
;; Actions
;;

(defn logout [] (aset js/window "location" "/"))

(defn get-friends2 []
  (chsk-send!
   [:user/friends-of-friends {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:friends2 app-state) (:friends2 resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (log* "Error in handle-enter:" resp)))
     (log* "Friends^2" (str @(:friends2 app-state))))))

(defn try-enter [hashed-id hashed-friends]
  (chsk-send!
   [:user/enter {:hashed-user hashed-id :hashed-friends hashed-friends}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:user-id app-state) (:found-user resp))
       (do (reset! app-error "There was an error with your login. Please try again.")
           (log* "Error in try-enter:" resp))))))

(defn- set-fake-facebooks-ids [hashed-id]
  (reset! (:user-hash app-state) hashed-id)
  (sente-register-init-callback! #(try-enter hashed-id ["TODO"]))
  (init-sente! hashed-id))

(defn- set-facebook-ids [response]
  (if (= (:status response) "connected")
    (let [user-fbid (get-in response [:authResponse :userID])
          hashed-id (cljs-hash.goog/hash :sha1 (str user-fbid))]
      (reset! (:user-fbid app-state) user-fbid)
      (reset! (:user-hash app-state) hashed-id)
      (log* "Connected with Facebook userID: " user-fbid)
      (log* "Hashed user: " hashed-id)
      (fb/api "/me/friends" {}
              (fn [{friends :data}]
                (let [friend-fbids (map :id friends)
                      hashed-friends (mapv #(cljs-hash.goog/hash :sha1 (str %)) friend-fbids)]
                  (reset! (:friend-fbids app-state) friend-fbids)
                  (reset! (:friend-hashes app-state) hashed-friends)
                  (log* "Friend IDs: " (str friend-fbids))
                  (log* "Hashed friends: " (str hashed-friends))
                  (sente-register-init-callback! #(try-enter hashed-id hashed-friends))
                  (init-sente! hashed-id)))))
    (log* "Not logged in: " (clj->js response))))

(defn get-user-requests []
  (chsk-send!
   [:user/buy-requests {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [requests (:buy-requests resp)]
         (log* "Received requests" requests)
         (reset! (:buy-requests app-state) requests))
       (do (reset! app-error "There was an error retrieving your previous buy requests. Please try again.")
           (log* "Error in get-user-requests:" resp))))))

(defn get-user-contracts []
  (chsk-send!
   [:user/contracts {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [contracts (:contracts resp)]
         (log* "Received contracts" contracts)
         (reset! (:contracts app-state) contracts)
         (if-let [contract-id (some #(and (= (:seller-id %) @(:user-id app-state)) (:id %))
                                    contracts)]
           (reset! (:display-contract app-state) contract-id)))
       (do (reset! app-error "There was an error retrieving your previous contracts. Please try again.")
           (log* "Error in get-user-contract:" resp))))))

(defn get-user-pending-notifications []
  (chsk-send!
   [:notification/get-pending {:user-hash @(:user-hash app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp))
       (when-let [notifications (:notifications resp)]
         (log* "Received notifications" notifications)
         (doseq [notif notifications]
           (swap! (:notifications app-state) conj notif)))
       (do (reset! app-error "There was an error retrieving your pending notifications. Please try again.")
           (log* "Error in get-user-pending-notifications:" resp))))))

(defn open-sell-offer [{:as vals :keys [currency min max]}]
  (chsk-send!
   [:offer/open {:user-id @(:user-id app-state) :min min :max max :currency currency}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:sell-offer app-state) (select-keys resp [:min :max]))
       (reset! app-error "There was an error opening the sell offer. Please try again.")))))

(defn get-active-sell-offer []
  (chsk-send!
   [:offer/get {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (let [offer (select-keys resp [:min :max])]
         (when (not-empty offer) (reset! (:sell-offer app-state) (into {} (for [[k v] offer] [k (common/currency-as-float (float v) :usd)])))))
       (reset! app-error "There was an error retrieving the sell offer.")))))

(defn close-sell-offer [callback]
  (chsk-send!
   [:offer/close {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (do (reset! (:sell-offer app-state) nil)
           (callback))
       (reset! app-error "There was an error closing the sell offer. Please try again.")))))

(defn get-sell-offer-matches []
  (chsk-send!
   [:offer/get-matches {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (let [offer-matches (:offer-matches resp)]
         (do (log* "Received offer matches" offer-matches)
             (doseq [m offer-matches] (swap! (:sell-offer-matches app-state) conj m))))
       (reset! app-error "There was an error retrieving the sell offer matches.")))))

(defn create-buy-request [amount callback]
  (chsk-send!
   [:buy-request/create {:user-id @(:user-id app-state)
                         :amount amount
                         :currency-buyer "usd"
                         :currency-seller "xbt"}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* "Buy request created successfully")
       (do (reset! app-error "There was an error creating the buy request. Please try again.")
           (log* "Error in create-buy-request:" resp)))
     (callback))))

(defn accept-buy-request [buy-request-id transfer-info]
  (chsk-send!
   [:buy-request/accept {:id buy-request-id :transfer-info transfer-info}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Buy request ID %d accepted" buy-request-id))
       (do (log* (gstring/format "Error accepting buy request ID %d" buy-request-id))
           (reset! app-error "There was an error accepting the buy request. Please try again."))))))

(defn decline-buy-request [buy-request-id]
  (chsk-send!
   [:buy-request/decline {:id buy-request-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Buy request ID %d declined" buy-request-id))
       (do (log* (gstring/format "Error declining buy request ID %d" buy-request-id))
           (reset! app-error "There was an error declining the buy request. Please try again."))))))

(defn mark-contract-sent [contract-id]
  (chsk-send!
   [:contract/mark-transfer-sent {:id contract-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Contract ID %d marked as transfer SENT" contract-id))
       (do (log* (gstring/format "Error marking contract ID %d as transfer SENT" contract-id))
           (reset! app-error "There was an error marking the contract. Please try again."))))))

(defn mark-contract-received [contract-id]
  (chsk-send!
   [:contract/mark-transfer-received {:id contract-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Contract ID %d marked as transfer RECEIVED" contract-id))
       (do (log* (gstring/format "Error marking contract ID %d as transfer RECEIVED" contract-id))
           (reset! app-error "There was an error marking the contract. Please try again."))))))

;;
;; Event Handlers
;;

;; App-level messages

(defmulti app-msg-handler first)

(defmethod app-msg-handler :default
  [app-msg]
  (log* "Unhandled app event: " (str app-msg)))

(defmethod app-msg-handler :sell-offer/match
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :sell-offer/match message")
    (swap! (:sell-offer-matches app-state) conj msg)))

(defmethod app-msg-handler :buy-request/create
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/create" msg)
    (swap! (:buy-requests app-state) conj msg)))

(defn find-buy-request [id] (first (keep-indexed #(when (= (:id %2) id) %1) @(:buy-requests app-state))))

(defmethod app-msg-handler :buy-request/match
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/match" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests app-state) assoc-in [found-idx :seller-id] (:seller-id msg))
      (do (reset! app-error "There was an error when matching the buy request. Please inform us of this event.")
          (log* "Error in buy-request/match" msg)))))

(defmethod app-msg-handler :buy-request/timed-out
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/timed-out" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests app-state) assoc-in [found-idx :seller-id] nil)
      (do (reset! app-error "There was an error when restarting the buy request. Please inform us of this event.")
          (log* "Error in buy-request/timed-out" msg)))))

(defmethod app-msg-handler :buy-request/accepted
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/accepted" msg)
    (try (swap! (:buy-requests app-state) (fn [q] (remove #(= (:id msg)) q)))
         (catch :default e
           (reset! app-error "There was an error when accepting the buy request. Please inform us of this event.")
           (log* "Error in buy-request/accepted:" e)))))

(defmethod app-msg-handler :buy-request/declined
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/declined" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests app-state) assoc-in [found-idx :seller-id] nil)
      (do (reset! app-error "There was an error when matching the buy request. Please inform us of this event.")
          (log* "Error in buy-request/declined" msg)))))

(defmethod app-msg-handler :contract/create
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/create" msg)
    (try (swap! (:contracts app-state) conj msg)
         (when (= (:seller-id msg) @(:user-id app-state))
           (reset! (:display-contract app-state) (:id msg)))
         (catch :default e
           (do (reset! app-error "There was an error when creating the contract. Please inform us of this event.")
               (log* "Error in contract/create" msg))))))

(defmethod app-msg-handler :contract/update
  [[_ {:keys [stage status id amount]}]]
  (reset! (:contracts app-state)
          (for [c @(:contracts app-state)]
            (if (= (:id c) id) (merge c {:stage stage :status status}) c))))

(defmethod app-msg-handler :contract/escrow-funded
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/escrow-funded" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (do (reset! (:display-contract app-state) nil)
          (swap! (:contracts app-state) assoc-in [found-idx :stage] "waiting-transfer"))
      (do (reset! app-error "There was an error in funding the Escrow. Please inform us of this event.")
          (log* "Error in contract/escrow-funded" msg)))))

;; (defmethod app-msg-handler :contract/waiting-transfer
;;   [[_ msg]]
;;   (if (:error msg)
;;     (log* "Error in :contract/waiting-transfer" msg)
;;     (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
;;       (swap! (:contracts app-state) assoc-in [found-idx :stage] "waiting-transfer")
;;       (do (reset! app-error "There was an error in the contract waiting-transfer stage. Please inform us of this event.")
;;           (log* "Error in contract/waiting-transfer" msg)))))

(defmethod app-msg-handler :contract/mark-transfer-sent-ack
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/mark-transfer-sent-ack" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (swap! (:contracts app-state) assoc-in [found-idx :transfer-sent] true)
      (do (reset! app-error "There was an error when marking the transfer as sent. Please inform us of this event.")
          (log* "Error in contract/mark-transfer-sent-ack" msg)))))

(defmethod app-msg-handler :contract/mark-transfer-received-ack
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/mark-transfer-received-ack" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (swap! (:contracts app-state) assoc-in [found-idx :transfer-received] true)
      (do (reset! app-error "There was an error when marking the transfer as received. Please inform us of this event.")
          (log* "Error in contract/mark-transfer-received-ack" msg)))))

(defmethod app-msg-handler :contract/holding-period
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/holding-period" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (swap! (:contracts app-state) assoc-in [found-idx :stage] "holding-period")
      (do (reset! app-error "There was an error when starting the contract holding period. Please inform us of this event.")
          (log* "Error in contract/holding-period" msg)))))

(defmethod app-msg-handler :contract/success
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/success" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (swap! (:contracts app-state) assoc-in [found-idx :stage] "contract-success")
      (do (reset! app-error "There was an error setting the contract as successful. Please inform us of this event.")
          (log* "Error in contract/holding-period" msg)))))

(defmethod app-msg-handler :contract/broken
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/broken" msg)
    (if-let [found-idx (find-in @(:contracts app-state) (:id msg))]
      (swap! (:contracts app-state) assoc-in [found-idx :stage] "contract-broken")
      (do (reset! app-error "There was an error setting the contract as broken. Please inform us of this event.")
          (log* "Error in contract/broken" msg)))))

(defmethod app-msg-handler :notification/create
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :notification/create" msg)
    (swap! (:notifications app-state) conj msg)))


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
    (log* "Handshake completed...")))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (log* "Push event from server: " (str ?data))
  (app-msg-handler ?data))

;;
;; UI Components
;;

(rum/defcs buy-dialog
  < rum/reactive (rum/local {:amount 1.0} ::input)
  [state]
  (let [input (::input state)
        valid-val #(and (number? %) (> % 0))
        xbt-usd (:xbt-usd (rum/react (:exchange-rates app-state)))
        total (* xbt-usd (:amount (rum/react input)))
        open? (= (rum/react (:ui-mode app-state)) :buy-dialog)]
    (if (<= (count @(:buy-requests app-state)) 10)
      (ui/dialog {:title "Buy Bitcoins"
                  :open open?
                  :modal true
                  :actions [(ui/flat-button {:label "Buy"
                                             :primary true
                                             :disabled (or (:processing (rum/react input)) (not (valid-val total)))
                                             :on-touch-tap
                                             (fn [e] (when (valid-val total)
                                                       (swap! input assoc :processing true)
                                                       (create-buy-request (:amount @input)
                                                                           #(do (reset! (:ui-mode app-state) :none)
                                                                                (swap! input assoc :processing false)))))})
                            (ui/flat-button {:label "Cancel"
                                             :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
                 [:div
                  [:div [:h4 "Bitcoin price: " xbt-usd " XBT/USD (Coinbase reference rate)"]
                   (ui/text-field {:id "amount"
                                   :autoFocus true
                                   :value (:amount (rum/react input))
                                   :on-change #(swap! input assoc :amount (.. % -target -value))
                                   :errorText (when (not (valid-val total)) "Invalid value")})
                   (when (> total 0)
                     (str "for " (round-currency total) " USD"))]
                  (when (:processing (rum/react input))
                    [:div
                     (ui/linear-progress {:size 60 :mode "indeterminate"
                                          :style {:margin "auto" :left "0" :right "0" :top "1.5rem"}})
                     [:h5 {:style {:text-align "center" :margin-top "2rem"}} "Initiating contract"]])])
      (ui/dialog {:title "Maximum number reached"
                  :open open?
                  :actions [(ui/flat-button {:label "OK"
                                             :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
                 "Maximum number of simultaneous open BUY requests reached."))))

(rum/defc sell-slider [currency [min-val max-val] on-change]
  (js/React.createElement js/Slider #js {:min 100
                                         :max 20000
                                         :range true
                                         :allowCross false
                                         :value #js [min-val max-val]
                                         :tipFormatter nil
                                         :onChange on-change}))

(rum/defcs sell-dialog
  < rum/reactive (rum/local {} ::ui-values)
  [state_]
  (let [offer-active? (boolean @(:sell-offer app-state))
        xbt-usd (:xbt-usd (rum/react (:exchange-rates app-state)))
        ui-values (::ui-values state_)
        min-val (or (:min @ui-values) (:min @(:sell-offer app-state)) 100)
        max-val (or (:max @ui-values) (:max @(:sell-offer app-state)) 20000)]
    (ui/dialog {:title (if offer-active? "Active offer" "Sell Bitcoins")
                :open (= (rum/react (:ui-mode app-state)) :sell-dialog)
                :modal true
                :actions [(when offer-active?
                            (ui/flat-button {:label "Remove"
                                             :on-touch-tap (fn []
                                                             (when (js/confirm "Are you sure?")
                                                               (close-sell-offer
                                                                #(reset! (:ui-mode app-state) :none))))}))
                          (ui/flat-button {:label (if offer-active? "Update" "Sell")
                                           :on-touch-tap (fn []
                                                           (open-sell-offer {:currency "usd" :min min-val :max max-val})
                                                           (reset! (:ui-mode app-state) :none))})
                          (ui/flat-button {:label "Back"
                                           :primary true
                                           :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
               [:div
                [:p "Offer transactions between " [:strong min-val]
                 " and " [:strong max-val] " USD (currently " [:strong (round-currency (/ min-val xbt-usd))]
                 " - " [:strong (round-currency (/ max-val xbt-usd))] " XBT)"]
                (sell-slider "usd" [min-val max-val] (fn [[mi ma]] (reset! ui-values {:min mi :max ma})))])))

(rum/defc menu-controls-comp
  < rum/reactive
  []
  [:div.main-menu
   [:h5.center (str "You can trade with " (count (rum/react (:friends2 app-state))) " partners")]
   (when-let [error (rum/react app-error)] [:h5.center.error error])
   [:div.center
    ;; TODO: hints http://kushagragour.in/lab/hint/
    (ui/raised-button {:label "BUY Bitcoins"
                       :disabled false #_(not (let [contracts (rum/react (:contracts app-state))]
                                                (or (= contracts :unknown) (empty? contracts))))
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode app-state) :buy-dialog)})
    (ui/raised-button {:label (if (rum/react (:sell-offer app-state)) "Change sell offer" "SELL Bitcoins")
                       :disabled false
                       :style {:margin "1rem"}
                       :on-touch-tap #(reset! (:ui-mode app-state) :sell-dialog)})]])

(rum/defc sell-offer-comp
  < rum/reactive
  []
  (when-let [sell-offer (rum/react (:sell-offer app-state))]
    (let [xbt-usd (:xbt-usd (rum/react (:exchange-rates app-state)))]
      [:div
       [:h4 {:style {:text-align "center"}} "Sell offer"]
       [:p.center "Offering transactions in the range "
        [:strong (:min sell-offer)] " to " [:strong (:max sell-offer)] " USD ("
        (round-currency (/ (:min sell-offer) xbt-usd)) " - "
        (round-currency (/ (:max sell-offer) xbt-usd)) " XBT)"]])))

(rum/defc request-listing-comp
  < rum/reactive
  []
  [:div
   [:h4 {:style {:text-align "center"}} "Buy requests"]
   [:div
    (let [requests (rum/react (:buy-requests app-state))]
      (cond
        (not requests)
        [:div "Retrieving requests..."
         (ui/linear-progress {:size 60 :mode "indeterminate"})]
        (empty? requests)
        [:p.center "No active requests"]
        :else
        (ui/list
         (for [req requests]
           (ui/list-item {:key (str "buy-request-item-" (:id req))
                          :primary-text (gstring/format "Buy request for %s %s"
                                                        (common/currency-as-float (:amount req) (:currency-seller req))
                                                        (clojure.string/upper-case (:currency-seller req)))
                          :secondary-text (gstring/format "ID: %d - %s"
                                                          (:id req)
                                                          (if (:seller-id req)
                                                            "PARTNER FOUND - WAITING SELLER ACTION"
                                                            "LOOKING FOR A PARTNER..."))})))))]])

(rum/defcs contract-dialog
  < rum/reactive
  (rum/local {:output-address "" :buyer-key ""} ::input)
  (rum/local nil ::user-key)
  [_state]
  (let [input (::input _state)
        user-key (::user-key _state)
        contract-id (rum/react (:display-contract app-state))]
    (when-let [contract (some #(and (= (:id %) contract-id) %) (rum/react (:contracts app-state)))]
      (case (:stage contract)

        "waiting-escrow"
        (when (am-i-seller? contract) ; make sure we are the seller
          (ui/dialog {:title "Contract Action Required"
                      :open true
                      :modal true
                      :actions [(ui/flat-button {:label "Close"
                                                 :primary true
                                                 :on-touch-tap #(reset! (:display-contract app-state) nil)})]}
                     [:div
                      [:div (if (:escrow-seller-has-key contract) {:style {:color "#bbb"}} {})
                       [:h3 "Step 1"]
                       (if (:escrow-seller-has-key contract)
                         [:p "The key has been extracted and is no longer available in our servers."]
                         [:div.flex-grid.aligner
                          [:div.col
                           (if (and (nil? (rum/react user-key)) (not (:escrow-seller-has-key contract)))
                             (ui/raised-button {:label "Get the Escrow Key"
                                                :primary true
                                                :on-touch-tap (fn [] (chsk-send!
                                                                      [:escrow/get-user-key {:id (:id contract) :role "seller"}] 5000
                                                                      #(if (and (sente/cb-success? %) (= :ok (:status %)))
                                                                         (reset! user-key (:escrow-user-key %))
                                                                         (log* "Error in escrow/get-user-key" %))))})
                             (ui/raised-button {:label "I have stored my key in a secure place"
                                                :primary true
                                                :on-touch-tap (fn [] (when (js/confirm "Please double-check the key. You will not be able to recover your funds without it.")
                                                                       (chsk-send!
                                                                        [:escrow/forget-user-key {:id (:id contract) :role "seller"}] 5000
                                                                        #(if (and (sente/cb-success? %) (= :ok (:status %)))
                                                                           (if-let [found-idx (find-in @(:contracts app-state) (:id contract))]
                                                                             (swap! (:contracts app-state) assoc-in [found-idx :escrow-seller-has-key] true)
                                                                             (log* "Error in escrow/forget-user-key (contract not found)" %))
                                                                           (log* "Error in escrow/forget-user-key" %)))))}))]
                          [:div.col [:div.center {:style {:font-size "small"}} (rum/react user-key)]]])]
                      [:h3 "Step 2: send " [:span {:style {:color "rgb(0, 188, 212)"}}
                                            (common/currency-as-float (:amount contract) (:currency-seller contract))
                                            " " (clojure.string/upper-case (:currency-seller contract))] " to the following Escrow address"]
                      [:div {:style {:background-color "#fff" :border-radius "2px"}}
                       [:div {:style {:color "#000" :padding "10px 0px 10px 0px" :text-align "center"}}
                        (:input-address contract)]]]))

        "waiting-transfer"
        (ui/dialog {:title "Contract Action Required"
                    :open true
                    :modal true
                    :actions [(ui/flat-button {:label "Close"
                                               :primary true
                                               :on-touch-tap #(reset! (:display-contract app-state) nil)})]}
                   (let [val-fiat [:strong (common/satoshi->btc (:amount contract)) " "
                                   (clojure.string/upper-case (:currency-seller contract))]]
                     (if (am-i-buyer? contract)
                       ;; Buyer dialog
                       [:div
                        [:div (if (:escrow-buyer-has-key contract) {:style {:color "#bbb"}} {})
                         [:h3 "Step 1"]
                         (if (:escrow-buyer-has-key contract)
                           [:p "The key has been extracted and is no longer available in our servers."]
                           [:div.flex-grid.aligner
                            [:div.col
                             (if (and (nil? (rum/react user-key)) (not (:escrow-buyer-has-key contract)))
                               (ui/raised-button {:label "Get the Escrow Key"
                                                  :primary true
                                                  :on-touch-tap (fn [] (chsk-send!
                                                                        [:escrow/get-user-key {:id (:id contract) :role "buyer"}] 5000
                                                                        #(if (and (sente/cb-success? %) (= :ok (:status %)))
                                                                           (reset! user-key (:escrow-user-key %))
                                                                           (log* "Error in escrow/get-user-key" %))))})
                               (ui/raised-button {:label "I have stored my key in a secure place"
                                                  :primary true
                                                  :on-touch-tap (fn [] (when (js/confirm "Please double-check the key. You will not be able to recover your funds without it.")
                                                                         (chsk-send!
                                                                          [:escrow/forget-user-key {:id (:id contract) :role "buyer"}] 5000
                                                                          #(if (and (sente/cb-success? %) (= :ok (:status %)))
                                                                             (if-let [found-idx (find-in @(:contracts app-state) (:id contract))]
                                                                               (swap! (:contracts app-state) assoc-in [found-idx :escrow-buyer-has-key] true)
                                                                               (log* "Error in escrow/forget-user-key (contract not found)" %))
                                                                             (log* "Error in escrow/forget-user-key" %)))))}))]
                            [:div.col (rum/react user-key)]])]
                        [:h3 "Step 2: send " [:span {:style {:color "rgb(0, 188, 212)"}} val-fiat] " to the seller account"]
                        [:pre {:style {:font-size "small"}} (:transfer-info contract)]
                        [:div
                         (ui/raised-button {:label "I've transferred the funds"
                                            :primary true
                                            :disabled (or (not (rum/react user-key)) (not (:escrow-buyer-has-key contract)))
                                            :on-touch-tap #(do (mark-contract-sent (:id contract))
                                                               (reset! (:display-contract app-state) nil))})]]
                       ;; Seller dialog
                       [:div
                        [:p "Please confirm here when you've received the funds in your bank account"]
                        (ui/raised-button {:label "I've received the funds"
                                           :primary true
                                           :on-touch-tap #(do (mark-contract-received (:id contract))
                                                              (reset! (:display-contract app-state) nil))})])))

        "contract-success"
        (ui/dialog {:title "Contract Action Required"
                    :open true
                    :modal true
                    :actions [(ui/flat-button {:label "Ok"
                                               :primary true
                                               :on-touch-tap
                                               (fn [] (chsk-send!
                                                       [:escrow/release-to-user {:id (:id contract)
                                                                                 :output-address (:output-address @input)
                                                                                 :escrow-user-key (:buyer-key @input)}]
                                                       5000
                                                       #(when (and (sente/cb-success? %) (= :ok (:status %)))
                                                          (if-let [found-idx (find-in @(:contracts app-state) (:id contract))]
                                                            (do (swap! (:contracts app-state) assoc-in [found-idx :output-address] (:output-address @input))
                                                                (reset! (:display-contract app-state) nil))
                                                            (do (reset! app-error "There was an error in requesting the Escrow funds. Please inform us of this event.")
                                                                (log* "Error calling contract/release-escrow-buyer" %))))))})
                              (ui/flat-button {:label "Cancel"
                                               :primary false
                                               :on-touch-tap #(reset! (:display-contract app-state) nil)})]}
                   [:div
                    [:h3 "Claim funds"]
                    [:h5 "Please provide the Destination Address and the Escrow Release Key in order to receive your Bitcoins"]
                    (ui/text-field {:id "output-address"
                                    :floating-label-text "Destination Address"
                                    :value (:output-address (rum/react input))
                                    :on-change #(swap! input assoc :output-address (.. % -target -value))})
                    [:br]
                    (ui/text-field {:id "buyer-key"
                                    :floating-label-text "Escrow Release Key"
                                    :value (:buyer-key (rum/react input))
                                    :on-change #(swap! input assoc :buyer-key (.. % -target -value))})])

        [:div "Nothing to do at the moment."]))))

(rum/defc contract-listing-comp
  < rum/reactive
  []
  [:div
   [:h4.center "Active contracts"]
   [:div
    (let [contracts (rum/react (:contracts app-state))]
      (cond
        (not contracts)
        [:div "Retrieving contracts..."
         (ui/linear-progress {:size 60 :mode "indeterminate"})]
        (empty? contracts)
        [:p.center "No active contracts"]
        :else
        (for [contract contracts]
          [:div {:key (:hash contract)}
           [:div
            [:div.column-half [:strong (if (am-i-seller? contract) "SELLER" "BUYER")]
             (str " // " (:hash contract))]
            (let [action-required [:div.column-half
                                   [:div.center.action-required {:on-click #(reset! (:display-contract app-state) (:id contract))}
                                    "ACTION REQUIRED"]]
                  waiting [:div.column-half [:div.center "WAITING"]]
                  done [:div.column-half [:div.center (if (am-i-buyer? contract) (str "RELEASED TO: " (:output-address contract)) "RELEASED TO BUYER")]]]
              (case (:stage contract)
                "waiting-escrow" (if (am-i-seller? contract) action-required waiting)
                "waiting-transfer" (if (am-i-buyer? contract)
                                     (if (or (not (:transfer-sent contract))
                                             (not (:escrow-buyer-has-key contract)))
                                       action-required waiting)
                                     (if (or (and (:transfer-sent contract) (not (:transfer-received contract)))
                                             (not (:escrow-seller-has-key contract)))
                                       action-required waiting))
                "contract-success" (if (and (am-i-buyer? contract) (empty? (:output-address contract)))
                                     action-required
                                     done)
                waiting))
            [:div {:style {:clear "both"}}]]
           [:div
            (ui/stepper {:active-step (case (:stage contract)
                                        "waiting-escrow" 0
                                        "waiting-transfer" (if (:transfer-sent contract) 2 1)
                                        "holding-period" 3
                                        ("contract-success" "contract-broken") 4)
                         :orientation "horizontal"}
                        (ui/step (ui/step-label "Escrow funding"))
                        (ui/step (ui/step-label "Send transfer"))
                        (ui/step (ui/step-label "Receive transfer"))
                        (ui/step (ui/step-label "Holding period")))]
           (case (:stage contract)
             "contract-success"
             [:div.contract-done [:div.contract-done-text "CONTRACT FINALIZED"]]
             "contract-broken"
             [:div.contract-done [:h6 "CONTRACT BROKEN"]]
             [:div.center {:style {:margin-bottom "5rem"}}
              (ui/flat-button {:label "Break contract"
                               :disabled (or (= (:stage contract) "contract-success")
                                             (= (:stage contract) "contract-broken"))
                               :style {:margin "0 1rem 0 1rem"}
                               :on-touch-tap #(js/alert "NOT IMPLEMENTED") #_#(js/confirm "Are you sure?")})])
           (contract-dialog)])))]])

(rum/defc generic-notifications
  < rum/reactive
  []
  (let [notifications (rum/react (:notifications app-state))
        current (peek notifications)]
    (ui/dialog {:title (or (:title current) "Notification")
                :open (boolean (not-empty notifications))
                :actions [(ui/flat-button {:label "OK"
                                           :primary true
                                           :on-touch-tap (or (:on-touch-tap current)
                                                             (fn [] (chsk-send!
                                                                     [:notification/ack {:user-hash @(:user-hash app-state)
                                                                                         :uuid (:uuid current)}]
                                                                     5000
                                                                     #(when (sente/cb-success? %)
                                                                        (swap! (:notifications app-state) pop)))))})]}
               [:div
                (for [chunk (clojure.string/split (:message current) #"\n")]
                  [:p chunk])])))

(rum/defcs sell-offer-matched-dialog
  < rum/reactive
  (rum/local true ::decline-lock)
  (rum/local {:number "" :name "" :address "" :iban "" :swift ""} ::account-info)
  [state_]
  (let [decline-lock (::decline-lock state_)
        pending-matches (rum/react (:sell-offer-matches app-state))
        current (peek pending-matches)
        account-info (::account-info state_)]
    (when current
      (ui/dialog {:title (gstring/format "Offer Matched for %f %s" (common/currency-as-float
                                                                    (:amount current)
                                                                    (:currency-seller current))
                                         (clojure.string/upper-case (:currency-seller current)))
                  :open (boolean (not-empty pending-matches))
                  :modal true
                  :actions [(ui/flat-button {:label "Accept"
                                             :primary true
                                             :on-touch-tap (fn []
                                                             (accept-buy-request
                                                              (:id current)
                                                              (clojure.string/join "\n" (map (fn [[k v]] (gstring/format "%s: %s" (name k) v)) @account-info)))
                                                             (swap! (:sell-offer-matches app-state) pop))})
                            (ui/flat-button {:label "Decline"
                                             :disabled (rum/react decline-lock)
                                             :on-touch-tap (fn []
                                                             (decline-buy-request (:id current))
                                                             (close-sell-offer #(swap! (:sell-offer-matches app-state) pop)))})]}
                 [:div
                  [:div.offer-matched-column
                   [:div
                    (ui/text-field {:id "account-number"
                                    :floating-label-text "Account Number"
                                    :value (:number (rum/react account-info))
                                    :on-change #(swap! account-info assoc :number (.. % -target -value))})
                    (ui/text-field {:id "account-name"
                                    :floating-label-text "Account Name"
                                    :value (:name (rum/react account-info))
                                    :on-change #(swap! account-info assoc :name (.. % -target -value))})
                    (ui/text-field {:id "account-address"
                                    :floating-label-text "Account Address"
                                    :value (:address (rum/react account-info))
                                    :on-change #(swap! account-info assoc :address (.. % -target -value))})]]
                  [:div.offer-matched-column
                   [:div
                    (ui/text-field {:id "account-iban"
                                    :floating-label-text "IBAN"
                                    :value (:iban (rum/react account-info))
                                    :on-change #(swap! account-info assoc :iban (.. % -target -value))})
                    (ui/text-field {:id "account-swift"
                                    :floating-label-text "SWIFT"
                                    :value (:swift (rum/react account-info))
                                    :on-change #(swap! account-info assoc :swift (.. % -target -value))})]]
                  [:div {:style {:position "absolute" :bottom 0 :margin-bottom "2rem"}}
                   [:h6  "To decline it, first check this, but your sell offer will be removed."
                    [:br]
                    (ui/checkbox {:label "I understand"
                                  :checked (not (rum/react decline-lock))
                                  :on-check #(reset! decline-lock (not %2))})]]]))))

(rum/defc footer
  []
  [:div.footer
   [:p.logout {:on-click logout} "Logout"]
   [:p.year (gstring/format "Cointrust © %d" (.getFullYear (js/Date.)))]])

(rum/defc login-comp
  []
  (ui/paper
   [:div {:style {:text-align "center"}}
    (ui/raised-button {:label "Ephemeral Login"
                       :style {:margin "1rem"}
                       :on-touch-tap
                       (fn [e]
                         (if hook-fake-id?_
                           (let [hashed-id "asdf" user-id 1]
                             (log* "Connected with fake user hash: " hashed-id)
                             (set-fake-facebooks-ids hashed-id))
                           (try
                             (fb/get-login-status
                              (fn [response]
                                (case (:status response)
                                  "connected"
                                  (set-facebook-ids response)
                                  (fb/login #(fb/get-login-status set-facebook-ids) {:scope "user_friends,public_profile,email"}))))
                             (catch :default e
                               (swap! (:notifications app-state) conj {:title "Error loading Facebook login"
                                                                       :message (str "Please check that you don't have a browser extension that disables the use of Social Logins.  Cointrust uses the social graph to find optimal matches for trading. /// Error /// " e)
                                                                       :on-touch-tap #(swap! (:notifications app-state) pop)}))
)))})
    (generic-notifications)]))

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
   (footer)])

(rum/defc app
  < rum/reactive
  []
  [:div {:style {:position "absolute"
                 :max-width "700px"
                 :margin "auto" :top "5rem" :bottom "0" :left "0" :right "0"}}
   (ui/mui-theme-provider
    {:mui-theme (get-mui-theme {:palette {:text-color (color :grey900)}})}
    [:div
     [:h1.title.center "COINTRUST"]
     [:h2.center "Friend of Friend Bitcoin Trading"]
     (if (rum/react (:user-hash app-state))
       (main-comp)
       (login-comp))])])

;;
;; Init
;;

(rum/mount (app) (js/document.getElementById "app"))

;; Run when we change the User ID
(add-watch (:user-id app-state) :got-user-id
           #(when %4
              (get-friends2)
              (get-active-sell-offer)
              (get-sell-offer-matches)
              (get-user-requests)
              (get-user-contracts)
              (get-user-pending-notifications)))
