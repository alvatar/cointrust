(ns oracle.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs-react-material-ui.core :refer [adapt-rum-class]])
  (:require [cljs.core.async :as async :refer (<! >! put! take! chan)]
            [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [cljs-react-material-ui.core :refer [get-mui-theme color] :as muic]
            [cljs-react-material-ui.icons :as ic]
            [cljs-react-material-ui.rum :as ui]
            [rum.core :as rum]
            [fb-sdk-cljs.core :as fb]
            [cljs-hash.goog :as gh]
            [goog.string :as gstring]
            ;; -----
            [oracle.common :as common]))


;;
;; Globals
;;

(goog-define *env* "dev")
(def hook-fake-id?_ (or (= *env* "dev") (= *env* "test")))

(js/console.log (str "Frontend environment is: " *env*))

(defonce app-error (atom nil))
(defonce app-state {:scene (atom "main-menu")
                    :ui-mode (atom :none)
                    :user-fbid (atom nil)
                    :user-hash (atom nil)
                    :user-id (atom nil)
                    :user-name (atom nil)
                    :friend-fbids (atom [])
                    :friend-hashes (atom [])
                    :friends2 (atom [])
                    :exchange-rates (atom {})
                    :seconds-since-last-exchange-rates-refresh (atom {})
                    :seconds-until-next-reconnect (atom nil)
                    :sell-offer (atom nil)
                    :sell-offer-matches (atom cljs.core/PersistentQueue.EMPTY)
                    :buy-requests (atom [])
                    :contracts (atom nil)
                    :notifications (atom cljs.core/PersistentQueue.EMPTY)
                    :display-contract (atom nil)
                    :server-time (atom nil)})

(def exchange-rates-refresh-interval 60)
(def max-allowed-transaction-in-usd 3000)

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
;; Utils
;;

(defn clj->json [ds] (.stringify js/JSON (clj->js ds)))

(defn log* [& args] (when true (js/console.log (clojure.string/join " " (map str args)))))

(defn open-page [url blank?]
  (if blank?
    (. js/window open url "_blank")
    (aset js/window "location" url)))

(defn find-in [col id] (first (keep-indexed #(when (= (:id %2) id) %1) col)))

(defn some-update [predicate f coll] (map (fn [x] (if (predicate x) (f x) x)) coll))

(defn update-contract [id f]
  (reset! (:contracts app-state)
          (doall (some-update #(= (:id %) id) f @(:contracts app-state)))))

(defn contract-time-left [contract server-time]
  (let [milliseconds->mins-formatted
        (fn [mil]
          (let [secs-raw (long (/ mil 1000))
                mins (quot secs-raw 60)
                secs (mod secs-raw 60)]
            (if (pos? secs-raw)
              (gstring/format "%s min. %s sec." mins secs)
              "no time")))
        time-diff (- server-time (:created contract))]
    (case (:stage contract)
      "waiting-escrow" ;; 60 mins
      (milliseconds->mins-formatted (- (* 60 60 1000) time-diff))
      "waiting-transfer" ;; 30 mins
      (milliseconds->mins-formatted (- (* 30 60 1000) time-diff))
      0)))

(defn close-display-contract! []
  (reset! (:display-contract app-state) nil))

;;
;; Helpers
;;

(defn round-currency
  ([val num-decimals]
   (let [pow (js/Math.pow 10 num-decimals)]
     (/ (long (* pow val)) pow)))
  ([val]
   (round-currency val 5)))

(defn am-i-buyer? [contract] (= @(:user-id app-state) (:buyer-id contract)))
(defn am-i-seller? [contract] (= @(:user-id app-state) (:seller-id contract)))
(defn waiting-transfer? [contract] (= (:stage contract) "waiting-transfer"))

;;
;; Widgets
;;

(rum/defc top-notification-bar
  < rum/reactive
  []
  (let [seconds-until (rum/react (:seconds-until-next-reconnect app-state))]
    (when seconds-until
      [:div.top-notification-bar.fadeIn
       {:style {:position "fixed"
                :top 0 :left 0 :right 0
                :height "3rem"
                :line-height "3rem"
                :background-color "rgb(0, 188, 212)"}}
       (gstring/format "Disconnected. Trying to reconnect in %s seconds..." seconds-until)])))

(defn mobile-overlay [open? & children]
  [:div {:style {:position "fixed"
                 :top 0 :left 0 :right 0 :bottom 0
                 :background-color "#fff"}
         :aria-hidden (not open?)}
   (into [:div {:style {:position :static}}]
         children)])

;;
;; Setup
;;

(enable-console-print!)

(defonce router_ (atom nil))
(def sente-callback-registry_ (atom []))

(defn sente-register-init-callback! [callback]
  (swap! sente-callback-registry_ conj callback))

(declare event-msg-handler)
(declare ch-chsk)

(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (log* "Initializing Sente client router...")
  (reset! router_ (sente/start-client-chsk-router! ch-chsk event-msg-handler)))

(def sente-reconnector (atom nil))

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
    (start-router!)
    (add-watch chsk-state :chsk-state-reconnect
               (fn [_1 _2 _3 _4]
                 (cond (and (:ever-opened? _4) (not (:open? _4)))
                       (swap! sente-reconnector
                              (fn [v] (or v (js/setInterval (fn []
                                                              (when (zero? (swap! (:seconds-until-next-reconnect app-state)
                                                                                  #(if (or (not %) (zero? %)) 10 (dec %))))
                                                                (sente/chsk-reconnect! chsk)))
                                                            1000))))
                       (:open? _4)
                       (do (swap! sente-reconnector #(when % (js/clearInterval %) nil))
                           (reset! (:seconds-until-next-reconnect app-state) nil)))))))

;;
;; Actions
;;

(defn push-error [message]
  (swap! (:notifications app-state) conj {:title "Error" :message message}))

(defn logout [] (aset js/window "location" "/"))

(defn get-friends2 []
  (chsk-send!
   [:user/friends-of-friends {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:friends2 app-state) (:friends2 resp))
       (do (push-error "There was an error with your login. Please try again.")
           (log* "Error in handle-enter:" resp)))
     (log* "Friends^2" (str @(:friends2 app-state))))))

(defn try-enter [user-fbid user-name hashed-id hashed-friends]
  (chsk-send!
   [:user/enter {:user-fbid user-fbid
                 :user-name user-name
                 :hashed-user hashed-id
                 :hashed-friends hashed-friends}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (do (log* "Try Enter: " resp)
           (reset! (:user-id app-state) (:found-user resp)))
       (do (push-error "There was an error with your login. Please try again.")
           (log* "Error in try-enter:" resp))))))

(defn- set-fake-facebooks-ids [hashed-id]
  (reset! (:user-hash app-state) hashed-id)
  #_(let [ffbids [10100642548250434 10106263879382352 10213129106885586 10210216755509404 145228535996960 145228535996960 145228535996960 145228535996960]]
    (reset! (:friend-fbids app-state) ffbids)
    #_(doseq [[f idx] (zipmap (take 8 f2bids) (range))]
      (fb/api (str "/" f "/picture")
              (fn [resp] (swap! (:friends2 app-state) conj {:id f
                                                            :name (str "Name " f)
                                                            :photo-url (get-in resp [:data :url])})))))
  (sente-register-init-callback! #(try-enter 1 "Alvatar" hashed-id ["TODO"]))
  (init-sente! hashed-id))

(defn- set-facebook-ids [response]
  (if (= (:status response) "connected")
    (let [user-fbid-str (get-in response [:authResponse :userID])
          user-fbid (js/Number (get-in response [:authResponse :userID]))
          hashed-id (cljs-hash.goog/hash :sha1 user-fbid-str)]
      (fb/api "/me/"
              (fn [resp]
                (reset! (:user-name app-state) (:name resp))
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
                            (sente-register-init-callback! #(try-enter user-fbid (:name resp) hashed-id hashed-friends))
                            (init-sente! hashed-id)))))))
    (log* "Not logged in: " (clj->js response))))



;; TODO: make desync differences smooth
(defn get-server-time []
  (chsk-send!
   [:server/time {}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (reset! (:server-time app-state) (:server-time resp))
       (log* "Error in server/clock" resp)))))

(defn get-exchange-rates []
  (chsk-send!
   [:currency/get-exchange-rates {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (reset! (:exchange-rates app-state) (get-in resp [:exchange-rates :rates]))
       (log* "Error in currency/get-exchange-rates" resp)))))

(defn get-user-requests []
  (chsk-send!
   [:user/buy-requests {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [requests (:buy-requests resp)]
         (log* "Received requests" requests)
         (reset! (:buy-requests app-state) requests))
       (do (push-error "There was an error retrieving your previous buy requests. Please try again.")
           (log* "Error in get-user-requests:" resp))))))

(defn get-user-contracts []
  (chsk-send!
   [:user/contracts {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [contracts (:contracts resp)]
         (log* "Received contracts" contracts)
         (reset! (:contracts app-state) contracts)
         ;; (if-let [contract-id (some #(and (= (:seller-id %) @(:user-id app-state)) (:id %))
         ;;                            contracts)]
         ;;   (reset! (:display-contract app-state) contract-id))
         )
       (do (push-error "There was an error retrieving your previous contracts. Please try again.")
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
       (do (push-error "There was an error retrieving your pending notifications. Please try again.")
           (log* "Error in get-user-pending-notifications:" resp))))))

(defn open-sell-offer [{:as vals :keys [currency min max premium]}]
  (chsk-send!
   [:offer/open {:user-id @(:user-id app-state) :min min :max max :currency currency :premium (long (* premium 100))}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (not (:error resp)))
       (reset! (:sell-offer app-state) (update resp :premium #(float (/ % 100))))
       (push-error "There was an error opening the sell offer. Please try again.")))))

(defn get-active-sell-offer []
  (chsk-send!
   [:offer/get {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (case (:status resp)
         :error
         (push-error (str "Internal error retrieving sell offer " (:message resp)))
         :no-offer nil
         (reset! (:sell-offer app-state)
                 {:min (common/currency-as-float (float (:min resp)) (:currency resp))
                  :max (common/currency-as-float (float (:max resp)) (:currency resp))
                  :currency (:currency resp)
                  :premium (float (/ (:premium resp) 100))}))
       (push-error "There was an error retrieving the sell offer.")))))

(defn close-sell-offer [callback]
  (chsk-send!
   [:offer/close {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (do (reset! (:sell-offer app-state) nil)
           (callback))
       (push-error "There was an error closing the sell offer. Please try again.")))))

(defn get-sell-offer-matches []
  (chsk-send!
   [:offer/get-matches {:user-id @(:user-id app-state)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (let [offer-matches (:offer-matches resp)]
         (do (log* "Received offer matches" offer-matches)
             (doseq [m offer-matches] (swap! (:sell-offer-matches app-state) conj
                                             (update m :premium #(float (/ % 100)))))))
       (push-error "There was an error retrieving the sell offer matches.")))))

(defn create-buy-request [amount callback]
  (chsk-send!
   [:buy-request/create {:user-id @(:user-id app-state)
                         :amount amount
                         :currency-buyer "usd"
                         :currency-seller "btc"}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* "Buy request created successfully")
       (do (push-error "There was an error creating the buy request. Please try again.")
           (log* "Error in create-buy-request:" resp)))
     (callback))))

(defn accept-buy-request [buy-request-id transfer-info]
  (chsk-send!
   [:buy-request/accept {:id buy-request-id :transfer-info transfer-info}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Buy request ID %d accepted" buy-request-id))
       (do (log* (gstring/format "Error accepting buy request ID %d" buy-request-id))
           (push-error "There was an error accepting the buy request. Please try again."))))))

(defn decline-buy-request [buy-request-id]
  (chsk-send!
   [:buy-request/decline {:id buy-request-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (log* (gstring/format "Buy request ID %d declined" buy-request-id))
       (do (log* (gstring/format "Error declining buy request ID %d" buy-request-id))
           (push-error "There was an error declining the buy request. Please try again."))))))

(defn mark-contract-received [contract-id]
  (chsk-send!
   [:contract/mark-transfer-received {:id contract-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (update-contract contract-id #(assoc % :transfer-received true))
       (log* (gstring/format "Contract ID %d marked as transfer RECEIVED" contract-id))))))

(defn break-contract [contract-id]
  (chsk-send!
   [:contract/break {:id contract-id}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (update-contract contract-id #(assoc % :stage "contract-broken"))
       (do (log* (gstring/format "Error breaking the contract ID %d" contract-id))
           (push-error "There was an error breaking the contract. Please try again."))))))

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
      (do (push-error "There was an error when matching the buy request. Please inform us of this event.")
          (log* "Error in buy-request/match" msg)))))

(defmethod app-msg-handler :buy-request/timed-out
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/timed-out" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests app-state) assoc-in [found-idx :seller-id] nil)
      (do (push-error "There was an error when restarting the buy request. Please inform us of this event.")
          (log* "Error in buy-request/timed-out" msg)))))

(defmethod app-msg-handler :buy-request/accepted
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/accepted" msg)
    (try (swap! (:buy-requests app-state) (fn [q] (remove #(= (:id msg)) q)))
         (catch :default e
           (push-error "There was an error when accepting the buy request. Please inform us of this event.")
           (log* "Error in buy-request/accepted:" e)))))

(defmethod app-msg-handler :buy-request/declined
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :buy-request/declined" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests app-state) assoc-in [found-idx :seller-id] nil)
      (do (push-error "There was an error when matching the buy request. Please inform us of this event.")
          (log* "Error in buy-request/declined" msg)))))

(defmethod app-msg-handler :contract/create
  [[_ msg]]
  (if (:error msg)
    (log* "Error in :contract/create" msg)
    (try (swap! (:contracts app-state) conj msg)
         ;; (when (= (:seller-id msg) @(:user-id app-state))
         ;;   (reset! (:display-contract app-state) (:id msg)))
         (catch :default e
           (do (push-error "There was an error when creating the contract. Please inform us of this event.")
               (log* "Error in contract/create" msg))))))

(defmethod app-msg-handler :contract/update
  [[_ {:keys [stage status id amount]}]]
  (reset! (:contracts app-state)
          (for [c @(:contracts app-state)]
            (if (= (:id c) id) (merge c {:stage stage :status status}) c))))

(defmethod app-msg-handler :contract/escrow-funded
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error in funding the Escrow. Please inform us of this event.")
        (log* "Error in contract/escrow-funded" msg))
    (update-contract (:id msg) #(assoc % :stage "waiting-transfer"))))

(defmethod app-msg-handler :contract/mark-transfer-received-ack
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error when marking the transfer as received. Please inform us of this event.")
        (log* "Error in contract/mark-transfer-received-ack" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/holding-period
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error when starting the contract holding period. Please inform us of this event.")
          (log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error setting the contract as successful. Please inform us of this event.")
        (log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-success"))))

(defmethod app-msg-handler :contract/broken
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error setting the contract as broken. Please inform us of this event.")
        (log* "Error in contract/broken" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken"))))

(defmethod app-msg-handler :contract/escrow-insufficient
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
        (log* "Error in contract/escrow-insufficient" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken/escrow-insufficient"))))

(defmethod app-msg-handler :contract/escrow-release-success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
        (log* "Error in contract/escrow-release-success" msg))
    (update-contract (:id msg) #(assoc % :escrow-release "<success>"))))

(defmethod app-msg-handler :contract/escrow-release-failure
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
        (log* "Error in contract/escrow-release-failed" msg))
    (update-contract (:id msg) #(assoc % :escrow-release "<failure>"))))


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
      ;;(log* "Channel socket state change: " new-state-map)
      )))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (log* "Handshake completed...")))


(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data ?reply-fn event]}]
  (when (and (= ?data [:chsk/ws-ping]) ?reply-fn)
    (?reply-fn {:chsk/ws-ping event}))
  (log* "Push event from server: " (str ?data))
  (app-msg-handler ?data))

;;
;; UI Components
;;

(rum/defcs buy-dialog
  < rum/reactive (rum/local {:amount 0.1 :currency "btc"} ::input)
  [state]
  (let [input (::input state)
        exchange-rates (rum/react (:exchange-rates app-state))
        parsed-val (js/parseFloat (:amount (rum/react input)))
        open? (= (rum/react (:ui-mode app-state)) :buy-dialog)
        currency (:currency (rum/react input))
        current-by-currency #(case (:currency @input)
                               "btc" (case %
                                       :btc parsed-val
                                       :usd (round-currency (* (:btc-usd exchange-rates) parsed-val) 2))
                               "usd" (case %
                                       :usd (round-currency parsed-val 2)
                                       :btc (* (:usd-btc exchange-rates) parsed-val)))
        content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                 [:div [:h4 "Price: 1 Bitcoin = $" (:btc-usd exchange-rates)]
                  [:h6 {:style {:margin-top "-1rem"}}
                   (gstring/format "Exchange rate will update in %s seconds" (- exchange-rates-refresh-interval
                                                                                (rum/react (:seconds-since-last-exchange-rates-refresh app-state))))]
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
                      "btc" (gstring/format "for %s USD" (round-currency (* (:btc-usd exchange-rates) parsed-val) 2))
                      "usd" (gstring/format "gets you %s BTC" (round-currency (* (:usd-btc exchange-rates) parsed-val)))))
                  [:h6 {:style {:margin-top "0.5rem"}} "Note: 1% fee and 1% seller premium will be paid from the total purchased."]
                  [:h4 {:style {:margin-top "-1rem"}}
                   (gstring/format "You will be paying $%s and receiving %s Bitcoin"
                                   (current-by-currency :usd)
                                   (* 0.99 0.99 (current-by-currency :btc)))]]
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
                                            (create-buy-request (current-by-currency :btc)
                                                                #(do (reset! (:ui-mode app-state) :none)
                                                                     (swap! input assoc :processing false)))))})
                 (ui/flat-button {:label "Cancel"
                                  :on-touch-tap #(reset! (:ui-mode app-state) :none)})]]
    (if (>= (count @(:buy-requests app-state)) 50)
      (ui/dialog {:title "Maximum number reached"
                  :open open?
                  :actions [(ui/flat-button {:label "OK"
                                             :on-touch-tap #(reset! (:ui-mode app-state) :none)})]}
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
  (let [offer-active? (boolean @(:sell-offer app-state))
        ui-values (::ui-values state_)
        sell-offer (:sell-offer app-state)
        min-val (or (:min (rum/react ui-values)) (:min (rum/react sell-offer)) 0.1)
        max-val (or (:max (rum/react ui-values)) (:max (rum/react sell-offer)) max-allowed-transaction-in-usd)
        parsed-min-val (let [p (js/Number min-val)] (when-not (js/isNaN p) p))
        parsed-max-val (let [p (js/Number max-val)] (when-not (js/isNaN p) p))
        currency (or (:currency (rum/react ui-values)) (:currency (rum/react sell-offer)) "usd")
        ex-rate (get (rum/react (:exchange-rates app-state)) (if (= currency "usd") :usd-btc :btc-usd))
        open? (= (rum/react (:ui-mode app-state)) :sell-dialog)
        content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                 [:div
                  [:h4 "How much would you like to sell? (Your coins will be sold at the market price, as listed on the top US exchange)"]
                  [:h4 "Bitcoin price: " (:btc-usd (rum/react (:exchange-rates app-state))) " BTC/USD"]
                  [:h6 {:style {:margin-top "-1rem"}}
                   (gstring/format "Exchange rate will update in %s seconds" (- exchange-rates-refresh-interval
                                                                                (rum/react (:seconds-since-last-exchange-rates-refresh app-state))))]
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
                       (str (round-currency (* ex-rate parsed-min-val) 2) " USD)")
                       (str (round-currency (* ex-rate parsed-min-val)) " BTC)"))])]
                 [:div.group
                  [:div {:style {:float "left"}}
                   (ui/text-field {:id "max"
                                   :floating-label-text "Max."
                                   :style {:margin-top "0px" :width "8rem"}
                                   :error-text (cond (not (pos? parsed-max-val)) "Invalid value"
                                                     (> parsed-min-val parsed-max-val) "Max should be larger than Min"
                                                     (> (case currency "usd" parsed-max-val (* ex-rate parsed-max-val)) max-allowed-transaction-in-usd)
                                                     (str "Max. of " max-allowed-transaction-in-usd " USD"))
                                   :value max-val
                                   :on-change #(swap! ui-values assoc :max (.. % -target -value))})]
                  (when parsed-max-val
                    [:div {:style {:float "left" :width "50%" :margin-top "2.5rem"}}
                     (clojure.string/upper-case currency) " - ("
                     (if (= currency "btc")
                       (str (round-currency (* ex-rate parsed-max-val) 2) " USD)")
                       (str (round-currency (* ex-rate parsed-max-val)) " BTC)"))])]]
        buttons [(when offer-active?
                   (ui/flat-button {:label "Remove"
                                    :key "offer-remove-button"
                                    :on-touch-tap (fn []
                                                    (when (js/confirm "Are you sure?")
                                                      (close-sell-offer
                                                       #(reset! (:ui-mode app-state) :none))))}))
                 (ui/flat-button {:label "Back"
                                  :key "offer-back-button"
                                  :on-touch-tap #(reset! (:ui-mode app-state) :none)})
                 (ui/flat-button {:label (if offer-active? "Update" "Sell")
                                  :primary true
                                  :key "offer-update-button"
                                  :disabled (not (and (pos? parsed-min-val) (pos? parsed-max-val)
                                                      (<= (case currency "usd" parsed-max-val (* ex-rate parsed-max-val)) 3000)
                                                      (> parsed-max-val parsed-min-val)))
                                  :on-touch-tap (fn []
                                                  (open-sell-offer {:currency currency :min min-val :max max-val :premium 0.01})
                                                  (reset! (:ui-mode app-state) :none))})]]
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
   [:h5.center (str "You can trade with " (count (rum/react (:friends2 app-state))) " people in your friend network")]
   [:div.center {:style {:margin-top "-1rem" :margin-bottom "1rem"}}
    (for [{:keys [id name photo-url]} (rum/react (:friends2 app-state))]
      [:span.profile-image.hint--bottom {:key (str "friend-photo-" id) :aria-label name}
       [:figure
        [:img {:src photo-url}]]])]
   (when-let [error (rum/react app-error)] [:h5.center.error error])
   [:div.center
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
    (let [currency (:currency sell-offer)
          ex-rate (get (rum/react (:exchange-rates app-state)) (if (= currency "usd") :usd-btc :btc-usd))]
      [:div
       [:h4 {:style {:text-align "center"}} "Sell offer"]
       [:p.center [:strong (:min sell-offer)] " to " [:strong (:max sell-offer)] " "
        (clojure.string/upper-case currency) " ("
        (round-currency (* (:min sell-offer) ex-rate)) " - "
        (round-currency (* (:max sell-offer) ex-rate)) " " (if (= currency "usd") "BTC" "USD") ")"
        [:br]]
       [:h6.center {:style {:margin-top "-0.8rem" :margin-bottom "1rem"}}
        "conversion excluding " (:premium sell-offer) "% premium"]])))

(rum/defc request-listing-comp
  < rum/reactive
  []
  [:div
   [:h4 {:style {:text-align "center"}} "Open requests"]
   [:div
    (let [requests (rum/react (:buy-requests app-state))]
      (cond
        (not requests)
        [:div "Retrieving requests..."
         (ui/linear-progress {:size 60 :mode "indeterminate"})]
        (empty? requests)
        [:p.center "No open requests"]
        :else
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
                                                            "LOOKING FOR A SELLER..."))})))))]])

(rum/defcs contract-dialog
  < rum/reactive
  (rum/local {:output-address "" :key ""} ::input)
  (rum/local nil ::user-key)
  (rum/local {} ::errors)
  [_state]
  (let [input (::input _state)
        user-key (::user-key _state)
        errors (::errors _state)
        contract-id (rum/react (:display-contract app-state))
        server-time (rum/react (:server-time app-state))]
    (when-let [contract (some #(and (= (:id %) contract-id) %) (rum/react (:contracts app-state)))]
      (let [time-left (contract-time-left contract server-time)
            escrow-release-dialog
            (fn [role]
             (let [buttons
                   [(ui/flat-button {:label "Ok"
                                     :primary true
                                     :on-touch-tap
                                     (fn []
                                       (if (or (= (:output-address @input) "") (= (:key @input) ""))
                                         (do (when (= (:output-address @input) "") (swap! errors assoc :output-address "Missing parameter"))
                                             (when (= (:key @input) "") (swap! errors assoc :key "Missing parameter")))
                                         (chsk-send!
                                          [:escrow/release-to-user {:id (:id contract)
                                                                    :user-role (name role)
                                                                    :output-address (:output-address @input)
                                                                    :escrow-user-key (:key @input)}]
                                          5000
                                          #(if (sente/cb-success? %)
                                             (case (:status %)
                                               :ok
                                               (do (update-contract
                                                    (:id contract)
                                                    (fn [c] (-> c
                                                                (assoc :output-address "<failure>")
                                                                (assoc :escrow-release "<processing>"))))
                                                   (close-display-contract!))
                                               :error-wrong-key
                                               (swap! errors assoc :key "Invalid Key")
                                               :error-wrong-address
                                               (swap! errors assoc :output-address "Invalid address")
                                               :error-missing-parameters
                                               (log* "Error calling contract/release-escrow-buyer" %))
                                             (do (reset! app-error "There was an error in requesting the Escrow funds. Please inform us of this event.")
                                                 (log* "Error calling contract/release-escrow-buyer" %))))))})
                    (ui/flat-button {:label "Cancel"
                                     :primary false
                                     :on-touch-tap close-display-contract!})]
                   content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                            [:h3 "Claim funds"]
                            [:h5 "Please provide the Destination Address and the Escrow Release Key in order to receive your Bitcoins"]
                            (ui/text-field {:id "output-address"
                                            :floating-label-text "Destination Address"
                                            :error-text (:output-address (rum/react errors))
                                            :value (:output-address (rum/react input))
                                            :style {:width "100%"}
                                            :on-change #(do (reset! errors {})
                                                            (swap! input assoc :output-address (.. % -target -value)))})
                            [:br]
                            (ui/text-field {:id "key"
                                            :floating-label-text "Escrow Release Key"
                                            :error-text (:key (rum/react errors))
                                            :value (:key (rum/react input))
                                            :style {:width "100%"}
                                            :on-change #(do (reset! errors {})
                                                            (swap! input assoc :key (.. % -target -value)))})]]
               (if (rum/react small-display?)
                 (mobile-overlay true content buttons)
                 (ui/dialog {:title (str "Contract ID: " (:human-id contract))
                             :open true
                             :modal true
                             :actions buttons}
                            content))))]

        (case (:stage contract)

          "waiting-escrow"
          (let [buttons [(ui/flat-button {:label "Close"
                                          :primary true
                                          :on-touch-tap close-display-contract!})]
                content [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                         [:div (if (:escrow-seller-has-key contract) {:style {:color "#bbb"}} {})
                          [:h3 (str "Time left: " time-left)]
                          [:h3 "Step 1: Get the Escrow key"]
                          (if (:escrow-seller-has-key contract)
                            [:p "The key has been extracted and is no longer available in our servers."]
                            [:div
                             [:div.center
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
                                                                           (fn [resp] (if (and (sente/cb-success? resp) (= :ok (:status resp)))
                                                                                        (update-contract (:id contract) #(assoc % :escrow-seller-has-key true))
                                                                                        (log* "Error in escrow/forget-user-key" resp))))))}))]
                             [:div.center.margin-2rem
                              [:div.center {:style {:font-size "small"}} (rum/react user-key)]]])]
                         [:h3 "Step 2: send " [:span {:style {:color "rgb(0, 188, 212)"}}
                                               (common/currency-as-float (:amount contract) (:currency-seller contract))
                                               " " (clojure.string/upper-case (:currency-seller contract))] " to the following Escrow address"]
                         [:div {:style {:background-color "#fff" :border-radius "2px"}}
                          [:div {:style {:color "#000" :padding "10px 0px 10px 0px" :text-align "center"}}
                           (:input-address contract)]]]]
            (when (am-i-seller? contract) ; make sure we are the seller
              (if (rum/react small-display?)
                (mobile-overlay true content buttons)
                (ui/dialog {:title (str "Contract ID: " (:human-id contract))
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
                              (gstring/format "This is a video contract confirming that I am conditionally agreeing to purchase %s USD of bitcoin."
                                              (common/currency-as-float (:amount contract) (:currency-seller contract)))
                              [:br]
                              (gstring/format "As long as the contract ID '%s' listed at cointrust.io shows that the contract is completed when I log into CoinTrust with my Facebook ID." (:human-id contract))])
                content
                (if (am-i-buyer? contract)
                  ;; Buyer dialog
                  [:div {:style {:padding (if (rum/react small-display?) "1rem" 0)}}
                   [:div (if (:escrow-buyer-has-key contract) {:style {:color "#bbb"}} {})
                    [:h3 "Step 1: get the Escrow key"]
                    (if (:escrow-buyer-has-key contract)
                      [:p "The key has been extracted and is no longer available in our servers."]
                      [:div
                       [:div.center
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
                       [:div.center.margin-2rem
                        [:div.center {:style {:font-size "small"}} (rum/react user-key)]]])]
                   [:h3 "Step 2: Click here to start a Facebook chat with " [:a {:href (str "https://facebook.com/messages/t/" (:seller-fb-id contract)) :target "_blank"} (:seller-name contract)]]
                   [:div.center
                    [:a.hint--bottom {:aria-label (str "Start a Facebook chat with " (:seller-name contract)) :href (str "https://facebook.com/messages/t/" (:seller-fb-id contract)) :target "_blank"}
                     [:img {:src (:seller-photo contract)}]]]
                   [:h3 "Step 3: On Facebook, " [:a {:href "https://www.youtube.com/watch?v=ZeAJDFgcCYA" :target "_blank"} "send a video message to Cedric."] " Record yourself reading this script."]
                   (legal-text :buyer)
                   [:h3 "Step 4: send "
                    [:span {:style {:color "rgb(0, 188, 212)"}}
                     (common/currency->symbol (:currency-buyer contract))
                     [:strong (round-currency
                               (* (common/currency-as-float (:amount contract)
                                                            (:currency-seller contract))
                                  (.-rep (:exchange-rate contract)))
                               2)]] " to @" (:transfer-info contract) " in Venmo"]]
                  ;; Seller dialog
                  [:div.padding-1rem
                   [:h3 "Step 1: Expect a video from the buyer reading the following text"]
                   (legal-text :seller)
                   [:h3 "Step 2: As soon as you receive a valid video in Facebook and the funds in Venmo, press the button below:"]
                   [:div.center
                    (ui/raised-button {:label "I've received the funds"
                                       :primary true
                                       :on-touch-tap #(do (mark-contract-received (:id contract))
                                                          (close-display-contract!))})]])
                buttons
                [(ui/flat-button {:label "Close"
                                  :primary true
                                  :on-touch-tap close-display-contract!})]]
            (if (rum/react small-display?)
              (mobile-overlay true content [:div {:style {:height "2rem"}}] buttons)
              (ui/dialog {:title (str "Contract ID: " (:human-id contract))
                          :open true
                          :modal true
                          :content-style {:width "500px"}
                          :actions buttons}
                         content)))

          "contract-success"
          (when (am-i-buyer? contract) (escrow-release-dialog :buyer))

          "contract-broken"
          (when (and (am-i-seller? contract) (:escrow-funded contract))
            (escrow-release-dialog :seller))

          "contract-broken/escrow-insufficient"
          (when (am-i-seller? contract) (escrow-release-dialog :seller))

          [:div "Unknown contract state. Please contact us."])))))

(rum/defc contract-listing-comp
  < rum/reactive
  []
  [:div
   [:h4.center.hint--bottom.hint--large {:aria-label "Smart contracts are computer protocols that facilitate, verify, or enforce the negotiation or performance of a contract, or that make a contractual clause unnecessary. Smart contracts often emulate the logic of contractual clauses." :style {:width "100%"}} "Smart Contracts"]
   [:div
    (let [_small-display? (rum/react small-display?)
          contracts (rum/react (:contracts app-state))
          server-time (rum/react (:server-time app-state))]
      (cond
        (not contracts)
        [:div "Retrieving contracts..."
         (ui/linear-progress {:size 60 :mode "indeterminate"})]
        (empty? contracts)
        [:p.center "No active contracts"]
        :else
        [:div
         (contract-dialog)
         (ui/list
          (for [contract contracts]
            (ui/list-item
             {:key (:hash contract)
              :primary-toggles-nested-list true
              :nested-items [(ui/list-item
                              {:key (str (:hash contract) "-children")}
                              [:div
                               (ui/stepper ((if _small-display? #(assoc % :connector nil) identity)
                                            {:active-step (case (:stage contract)
                                                            "waiting-escrow" 1
                                                            "waiting-transfer" 2
                                                            ("contract-success" "contract-broken" "contract-broken/escrow-insufficient") 3)
                                             :orientation (if _small-display? "vertical" "horizontal")
                                             :style (if _small-display? {:width "12rem" :margin "0 auto"} {})})
                                           (ui/step (ui/step-label "Contract creation"))
                                           (ui/step (ui/step-label "Escrow funding"))
                                           (ui/step (ui/step-label "Contract execution")))
                               [:p.center [:strong "Contract ID: "] (:human-id contract)]
                               (let [explorer-url (case *env*
                                                    "production" "https://blockchain.info/tx/"
                                                    "staging" "https://tbtc.blockr.io/tx/info/"
                                                    "")]
                                 [:div.center
                                  [(if (:input-tx contract) :a.tx-link :a.tx-link-shadow) (when (:input-tx contract) {:href (str explorer-url (:input-tx contract)) :target "_blank"})
                                   [:strong "escrow input transaction"]]
                                  [:span.tx-link " / "]
                                  [(if (:output-tx contract) :a.tx-link :a.tx-link-shadow) (when (:output-tx contract) {:href (str explorer-url (:output-tx contract)) :target "_blank"})
                                   [:strong "escrow output transaction"]]])
                               #_(when (:output-tx contract) [:a.tx-link {:href (str "https://tbtc.blockr.io/tx/info/" (:output-tx contract))  :target "_blank"}
                                                              [:strong "Escrow Output Transaction"]])
                               (when (or (= (:stage contract) "waiting-transfer")
                                         (and (= (:stage contract) "waiting-escrow")
                                              (am-i-seller? contract)))
                                 [:div.center {:style {:margin-bottom "5rem"}}
                                  (ui/flat-button {:label "Break contract"
                                                   :style {:margin "0 1rem 0 1rem"}
                                                   :on-touch-tap #(when (js/confirm "Are you sure?") (break-contract (:id contract)))})])])]}
             [:div.hint--bottom {:aria-label (gstring/format "Waiting for %s to deposit Bitcoins into this Smart Contract" (:seller-name contract))
                                 :style {:width "100%"}}
              [(if _small-display? :div.center :div.column-half)
               [:strong (if (am-i-seller? contract) "SELLER" "BUYER")]
               (gstring/format " // %s BTC " (common/satoshi->btc (:amount contract)))
               (when-not _small-display?
                 [:span {:style {:font-size "x-small" :display "table-cell" :vertical-align "middle"}} (str "Contract ID: " (:human-id contract))])]
              (let [action-required (fn [text]
                                      [(if _small-display? :div.center.margin-1rem-top :div.column-half)
                                       [:div.center.action-required {:on-click #(reset! (:display-contract app-state) (:id contract))} text]])
                    status-class (if _small-display? :div.center.margin-1rem-top :div.column-half)
                    waiting [status-class [:div.center "WAITING"]]
                    time-left (contract-time-left contract server-time)]
                (case (:stage contract)
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
                                         "<fresh>" (action-required "RELEASE FUNDS")
                                         "<failure>" (action-required "FAILED RELEASE")
                                         "<success>" [status-class [:div.center (str "RELEASED TO: " (:output-address contract))]]
                                         "<processing>" [status-class [:div.center (str "RELEASING TO: " (:output-address contract))]]
                                         [status-class [:div.center "UNKNOWN ESCROW STATE"]]))
                  "contract-broken" (if (am-i-buyer? contract)
                                      (if (:escrow-funded contract)
                                        [status-class [:div.center "PERFORM CHARGEBACK"]]
                                        [status-class [:div.center "CONTRACT BROKEN"]])
                                      (if (:escrow-funded contract)
                                        (case (:escrow-release contract)
                                          "<fresh>" (action-required "RELEASE FUNDS")
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
              [:div {:style {:clear "both"}}]])))]))]])

(rum/defc generic-notifications
  < rum/reactive
  []
  (let [notifications (rum/react (:notifications app-state))
        current (peek notifications)]
    (ui/snackbar {:open (boolean (not-empty notifications))
                  :message (str (:title current) " " (:message current))
                  :on-request-close #(swap! (:notifications app-state) pop)
                  ;; :actions [(ui/flat-button {:label "OK"
                  ;;                            :primary true
                  ;;                            :on-touch-tap (or (:on-touch-tap current)
                  ;;                                              (fn [] (chsk-send!
                  ;;                                                      [:notification/ack {:user-hash @(:user-hash app-state)
                  ;;                                                                          :uuid (:uuid current)}]
                  ;;                                                      5000
                  ;;                                                      #(when (sente/cb-success? %)
                  ;;                                                         (swap! (:notifications app-state) pop)))))})]
                  })))

(rum/defcs sell-offer-matched-dialog
  < rum/reactive
  (rum/local {:name ""} ::account-info)
  [state_]
  (let [pending-matches (rum/react (:sell-offer-matches app-state))
        open? (boolean (not-empty pending-matches))
        current (peek pending-matches)
        account-info (::account-info state_)]
    (when current
      (let [buttons [(ui/flat-button {:label "Accept"
                                      :primary true
                                      :disabled (clojure.string/blank? (:name (rum/react account-info)))
                                      :on-touch-tap (fn []
                                                      (accept-buy-request
                                                       (:id current)
                                                       ;;(clojure.string/join "\n" (map (fn [[k v]] (gstring/format "%s: %s" (name k) v)) @account-info))
                                                       (:name @account-info))
                                                      (swap! (:sell-offer-matches app-state) pop))})
                     (ui/flat-button {:label "Decline"
                                      :on-touch-tap (fn []
                                                      (decline-buy-request (:id current))
                                                      (close-sell-offer #(swap! (:sell-offer-matches app-state) pop)))})]
            ;; Here we calculate the amount the seller is going to put in the escrow
            title (gstring/format "Offer Matched for %f %s" (* (common/currency-as-float
                                                                (:amount current)
                                                                (:currency-seller current))
                                                               (- 1 (/ (:premium current) 100)))
                                  (clojure.string/upper-case (:currency-seller current)))
            div-account-name [:div.left [:span {:style {:margin-right "2px"}} "@"]
                              (ui/text-field {:id "account-name"
                                              :floating-label-text "Venmo ID"
                                              :value (:name (rum/react account-info))
                                              :on-change #(swap! account-info assoc :name (.. % -target -value))})]]
        (if (rum/react small-display?)
          (mobile-overlay
           open?
           [:div.padding-1rem
            [:h3 title]
            [:div div-account-name]
            buttons])
          (ui/dialog {:title title
                      :open open?
                      :modal true
                      :actions buttons
                      :content-style {:width "340px"}}
                     [:div.center div-account-name]))))))

(rum/defc footer
  < rum/reactive
  []
  [:div.footer
   [:p.logout (gstring/format "Logged in as %s / " (rum/react (:user-name app-state)))
    [:span {:style {:cursor "pointer"} :on-click logout} "Logout"]]
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
                                     (fb/login #(fb/get-login-status set-facebook-ids) {:scope "user_friends,public_profile"}))))
                                (catch :default e (reset! fb-error (str e))))))}))]
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
                         {:key "qa-text-1-nested"
                          :disabled true}
                         [:ul
                          [:li "Cointrust makes it easy to buy & sell Bitcoin instantly and risk- free by using Facebook, Venmo, and "
                           [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts"]]])]}
        [:h3 "What is Cointrust?"])
       (ui/list-item
        {:key "qa-text-2"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-2-nested"
                          :disabled true}
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
                         {:key "qa-text-3-nested"
                          :disabled true}
                         [:ul
                          [:li "Cointrust makes Bitcoin purchases risk-free for both buyers and sellers through the use of " [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts."]]
                          [:li "Cointrust acts as a notary, confirming both buyer and seller have lived up to their part of the agreement. And protecting the other person if they don’t."]])]}
        [:h3 "Risk Free"])
       (ui/list-item
        {:key "qa-text-4"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-4-nested"
                          :disabled true}
                         [:ul
                          [:li "By using Cointrust, you can sell Bitcoin without the risk of chargebacks."]
                          [:li "Dollar Payment are reversible (Venmo, Paypal, Credit Card, Bank Transfer). "]
                          [:li "But Bitcoin payments aren’t. This is why buying bitcoin can be PAINFULLY slow."]
                          [:li "By using " [:span.hint--bottom.hint--large smart-contract-fields "Smart Contracts"] ", Cointrust acts like a digital notary confirming all the information you need as a buyer or seller to be protected and either perform or reverse a chargeback."]])]}
        [:h3 "What about chargebacks?"])
       (ui/list-item
        {:key "qa-text-5"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-5-nested"
                          :disabled true}
                         [:ul
                          [:li "Bitcoin buyers are guaranteed to get what they pay for."]
                          [:li "We hold bitcoins in a " [:span.hint--bottom.hint--large smart-contract-fields "smart contracts"]
                           " (think of it like an escrow account) and confirm the amount the buyer will receive."]])]}
        [:h3 "Am I protected if I’m buying Bitcoin?"])
       (ui/list-item
        {:key "qa-text-6"
         :primary-toggles-nested-list true
         :nested-items [(ui/list-item
                         {:key "qa-text-6-nested"
                          :disabled true}
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
  (let [user (rum/react (:user-hash app-state))]
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

;;
;; Init
;;

(try
  (fb/load-sdk (fn []
                 (js/console.log "Facebook lib loaded")
                 (fb/init {:appId (case *env*
                                    "production" "1131377006981108"
                                    "staging" "1382322701791260"
                                    ("test" "dev") "1214574721994669")
                           :status true
                           :cookies false
                           :xfbml true
                           :version "v2.8"})))
  (catch :default e (js/console.log e)))

(rum/mount (app) (js/document.getElementById "app"))

;;
;; Watchers
;;

;; Run when we change the User ID
(add-watch (:user-id app-state) :got-user-id
           (fn [_1 _2 _3 _4]
             (when _4
               (get-server-time)
               (js/setInterval get-server-time 20000)
               (js/setInterval #(swap! (:server-time app-state) (partial + 1000)) 1000)
               (get-exchange-rates)
               (js/setInterval #(let [a (:seconds-since-last-exchange-rates-refresh app-state)]
                                  (if (< @a exchange-rates-refresh-interval) (swap! a inc) (do (get-exchange-rates) (reset! a 0))))
                               1000)
               (get-friends2)
               (get-active-sell-offer)
               (get-sell-offer-matches)
               (get-user-requests)
               (get-user-contracts)
               (get-user-pending-notifications))))

(add-watch (:friends2 app-state) :got-friends2
           (fn [_1 _2 _3 _4]
             (doseq [[f idx] (zipmap (take 8 @(:friends2 app-state)) (range))]
               (fb/api (str "/" (:fb-id f) "/picture")
                       #(if-let [photo-url (get-in % [:data :url])]
                          (swap! (:friends2 app-state) assoc-in [idx :photo-url] photo-url)
                          (log* %))))
             (remove-watch (:friends2 app-state) :got-friends2)))

(defn get-photo-for-contract! [contract]
  (fb/api (str "/" (:seller-fb-id contract) "/picture")
          (fn [resp]
            (if-let [photo-url (get-in resp [:data :url])]
              (update-contract (:id contract) #(assoc % :seller-photo photo-url))
              (log* resp)))))

(add-watch (:contracts app-state) :fetch-contract-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2] (when-not (:seller-photo c) (get-photo-for-contract! c)))))
