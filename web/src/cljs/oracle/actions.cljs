(ns oracle.actions
  (:require [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [goog.string :as gstring]
            [cljs-hash.goog :as gh]
            [fb-sdk-cljs.core :as fb]
            ;; -----
            [oracle.common :as common]
            [oracle.globals :as globals]
            [oracle.state :as state]
            [oracle.utils :as utils]
            [oracle.network :as network]))

(declare event-msg-handler)

;;
;; Helpers
;;

(defn update-sell-offer [id f]
  (reset! (:sell-offer-matches state/app)
          ;; Using vector
          (doall (utils/some-updatev #(= (:id %) id) f @(:sell-offer-matches state/app)))))

(defn update-buy-request [id f]
  (reset! (:buy-requests state/app)
          (doall (utils/some-update #(= (:id %) id) f @(:buy-requests state/app)))))

(defn update-contract [id f]
  (reset! (:contracts state/app)
          (doall (utils/some-update #(= (:id %) id) f @(:contracts state/app)))))

;;
;; Actions
;;

(defn push-error [message]
  (swap! (:notifications state/app) conj {:title message :message ""}))

(defn get-server-time []
  (network/send!
   [:server/time {}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (reset! (:server-time state/app) (:server-time resp))
       (utils/log* "Error in server/clock" resp)))))

(defn get-exchange-rates []
  (network/send!
   [:currency/get-exchange-rates {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (reset! (:exchange-rates state/app) (get-in resp [:exchange-rates :rates]))
       (utils/log* "Error in currency/get-exchange-rates" resp)))))

(defn get-friends2 []
  (network/send!
   [:user/friends-of-friends {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:friends2 state/app) (:friends2 resp))
       (do (push-error "Error retrieving data. Please relogin.")
           (utils/log* "Error in get-friends2:" resp)))
     (utils/log* "Friends^2:" (str @(:friends2 state/app))))))

(defn get-user-requests []
  (network/send!
   [:user/buy-requests {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [requests (:buy-requests resp)]
         (utils/log* "Received requests" requests)
         (reset! (:buy-requests state/app) requests))
       (do (push-error "Error retrieving data. Please relogin.")
           (utils/log* "Error in get-user-requests:" resp))))))

(defn get-user-contracts []
  (network/send!
   [:user/contracts {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [contracts (:contracts resp)]
         (utils/log* "Received contracts" contracts)
         (reset! (:contracts state/app) contracts)
         ;; (if-let [contract-id (some #(and (= (:seller-id %) @(:user-id state/app)) (:id %))
         ;;                            contracts)]
         ;;   (reset! (:display-contract state/app) contract-id))
         )
       (do (push-error "Error retrieving data. Please relogin.")
           (utils/log* "Error in get-user-contract:" resp))))))

(defn get-user-pending-notifications []
  (network/send!
   [:notification/get-pending {:user-hash @(:user-hash state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp))
       (when-let [notifications (:notifications resp)]
         (utils/log* "Received notifications" notifications)
         (doseq [notif notifications]
           (swap! (:notifications state/app) conj notif)))
       (do (push-error "There was an error retrieving your pending notifications. Please try again.")
           (utils/log* "Error in get-user-pending-notifications:" resp))))))

(defn open-sell-offer [{:as vals :keys [currency min max premium]}]
  (network/send!
   [:offer/open {:user-id @(:user-id state/app) :min min :max max :currency currency :premium (long (* premium 100))}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (not (:error resp)))
       (reset! (:sell-offer state/app) (update resp :premium #(float (/ % 100))))
       (push-error "There was an error opening the sell offer. Please try again.")))))

(defn get-active-sell-offer []
  (network/send!
   [:offer/get {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (case (:status resp)
         :error
         (push-error (str "Internal error retrieving sell offer " (:message resp)))
         :no-offer nil
         (reset! (:sell-offer state/app)
                 {:min (common/currency-as-float (float (:min resp)) (:currency resp))
                  :max (common/currency-as-float (float (:max resp)) (:currency resp))
                  :currency (:currency resp)
                  :premium (float (/ (:premium resp) 100))}))
       (push-error "There was an error retrieving the sell offer.")))))

(defn close-sell-offer [callback]
  (network/send!
   [:offer/close {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (do (reset! (:sell-offer state/app) nil)
           (callback))
       (push-error "There was an error closing the sell offer. Please try again.")))))

(defn get-sell-offer-matches []
  (network/send!
   [:offer/get-matches {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (let [offer-matches (:offer-matches resp)]
         (do (utils/log* "Received offer matches" offer-matches)
             (doseq [m offer-matches] (swap! (:sell-offer-matches state/app) conj
                                             (update m :premium #(float (/ % 100)))))))
       (push-error "There was an error retrieving the sell offer matches.")))))

(defn create-buy-request [amount callback]
  (network/send!
   [:buy-request/create {:user-id @(:user-id state/app)
                         :amount amount
                         :currency-buyer "usd"
                         :currency-seller "btc"}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (utils/log* "Buy request created successfully")
       (do (push-error "There was an error creating the buy request. Please try again.")
           (utils/log* "Error in create-buy-request:" resp)))
     (callback))))

(defn accept-buy-request [buy-request-id transfer-info]
  (network/send!
   [:buy-request/accept {:id buy-request-id :transfer-info transfer-info}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (utils/log* (gstring/format "Buy request ID %d accepted" buy-request-id))
       (do (utils/log* (gstring/format "Error accepting buy request ID %d" buy-request-id))
           (push-error "There was an error accepting the buy request. Please try again."))))))

(defn decline-buy-request [buy-request-id]
  (network/send!
   [:buy-request/decline {:id buy-request-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (utils/log* (gstring/format "Buy request ID %d declined" buy-request-id))
       (do (utils/log* (gstring/format "Error declining buy request ID %d" buy-request-id))
           (push-error "There was an error declining the buy request. Please try again."))))))

(defn mark-contract-received [contract-id]
  (network/send!
   [:contract/mark-transfer-received {:id contract-id}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (update-contract contract-id #(assoc % :transfer-received true))
       (utils/log* (gstring/format "Contract ID %d marked as transfer RECEIVED" contract-id))))))

(defn break-contract [contract-id]
  (network/send!
   [:contract/break {:id contract-id}] 5000
   (fn [resp]
     (if (sente/cb-success? resp)
       (update-contract contract-id #(assoc % :stage "contract-broken"))
       (do (utils/log* (gstring/format "Error breaking the contract ID %d" contract-id))
           (push-error "There was an error breaking the contract. Please try again."))))))


;;
;; Data retrieval
;;

(defn retrieve-app-data []
  (get-server-time)
  (get-exchange-rates)
  (get-friends2)
  (get-active-sell-offer)
  (get-sell-offer-matches)
  (get-user-requests)
  (get-user-contracts)
  (get-user-pending-notifications))

(defn retrieve-app-data-cycle-start! []
  (js/setInterval get-server-time 20000)
  (js/setInterval #(swap! (:server-time state/app) (partial + 1000)) 1000)
  (js/setInterval #(let [a (:seconds-since-last-exchange-rates-refresh state/app)]
                     (if (< @a globals/exchange-rates-refresh-interval)
                       (swap! a inc)
                       (do (get-exchange-rates) (reset! a 0))))
                  1000))

(add-watch (:friends2 state/app) :got-friends2
           (fn [_1 _2 _3 _4]
             (doseq [[f idx] (zipmap (take 8 @(:friends2 state/app)) (range))]
               (fb/api (str "/" (:fb-id f) "/picture")
                       #(if-let [photo-url (get-in % [:data :url])]
                          (swap! (:friends2 state/app) assoc-in [idx :photo-url] photo-url)
                          (utils/log* %))))
             (remove-watch (:friends2 state/app) :got-friends2)))

(defn get-photo-for! [obj type role]
  (when-let [id ((keyword (str (name role) "-fb-id")) obj)]
    (fb/api (str "/" id "/picture")
            (fn [resp]
              (if-let [photo-url (get-in resp [:data :url])]
                ((case type
                   :sell-offer update-sell-offer
                   :buy-request update-buy-request
                   :contract update-contract)
                 (:id obj) #(assoc % (keyword (str (name role) "-photo")) photo-url))
                (utils/log* "Couldn't retrieve photo from Facebook"))))))

(add-watch (:sell-offer-matches state/app) :fetch-sell-offer-match-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when (and (:buyer-id c) (not (:buyer-photo c)))
                 (get-photo-for! c :sell-offer :buyer)))))

(add-watch (:buy-requests state/app) :fetch-buy-requests-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when (and (:seller-id c) (not (:seller-photo c)))
                 (get-photo-for! c :buy-request :seller)))))

(add-watch (:contracts state/app) :fetch-contract-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when-not (:seller-photo c) (get-photo-for! c :contract :seller))
               (when-not (:buyer-photo c) (get-photo-for! c :contract :buyer)))))

;;
;; Event Handlers
;;

;; App-level messages

(defmulti app-msg-handler first)

(defmethod app-msg-handler :default
  [app-msg]
  (utils/log* "Unhandled app event: " (str app-msg)))

(defmethod app-msg-handler :sell-offer/match
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :sell-offer/match message")
    (swap! (:sell-offer-matches state/app) conj msg)))

(defmethod app-msg-handler :buy-request/create
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/create" msg)
    (swap! (:buy-requests state/app) conj msg)))

(defmethod app-msg-handler :buy-request/match
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error matching the buy request.")
        (utils/log* "Error in buy-request/match" msg))
    (update-buy-request (:id msg) #(-> %
                                       (assoc :seller-id (:seller-id msg))
                                       (assoc :seller-fb-id (:seller-fb-id msg))
                                       (assoc :seller-name (:seller-name msg))))))

(defmethod app-msg-handler :buy-request/timed-out
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error restarting the buy request.")
        (utils/log* "Error in buy-request/timed-out" msg))
    (update-buy-request (:id msg) #(dissoc % :seller-id))))

(defmethod app-msg-handler :buy-request/accepted
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/accepted" msg)
    (try (swap! (:buy-requests state/app) (fn [q] (remove #(= (:id msg)) q)))
         (catch :default e
           (push-error "Error accepting the buy request. Please inform us of this event.")
           (utils/log* "Error in buy-request/accepted:" e)))))

(defmethod app-msg-handler :buy-request/declined
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error declining the buy request.")
        (utils/log* "Error in buy-request/declined" msg))
    (update-buy-request (:id msg) #(dissoc % :seller-id))))

(defmethod app-msg-handler :contract/create
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :contract/create" msg)
    (try (swap! (:contracts state/app) conj msg)
         ;; (when (= (:seller-id msg) @(:user-id state/app))
         ;;   (reset! (:display-contract state/app) (:id msg)))
         (catch :default e
           (do (push-error "Error creating the contract.")
               (utils/log* "Error in contract/create" msg))))))

(defmethod app-msg-handler :contract/update
  [[_ {:keys [stage status id amount]}]]
  (reset! (:contracts state/app)
          (for [c @(:contracts state/app)]
            (if (= (:id c) id) (merge c {:stage stage :status status}) c))))

(defmethod app-msg-handler :contract/escrow-funded
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error in funding the Escrow.")
        (utils/log* "Error in contract/escrow-funded" msg))
    (update-contract (:id msg) #(assoc % :stage "waiting-transfer"))))

(defmethod app-msg-handler :contract/mark-transfer-received-ack
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error marking the transfer as received.")
        (utils/log* "Error in contract/mark-transfer-received-ack" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/holding-period
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error starting the contract holding period.")
          (utils/log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Erro finalizing the contract.")
        (utils/log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-success"))))

(defmethod app-msg-handler :contract/broken
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error breaking the contract.")
        (utils/log* "Error in contract/broken" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken"))))

(defmethod app-msg-handler :contract/escrow-insufficient
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error releasing the Escrow.")
        (utils/log* "Error in contract/escrow-insufficient" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken/escrow-insufficient"))))

(defmethod app-msg-handler :contract/escrow-release-success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error releasing the Escrow.")
        (utils/log* "Error in contract/escrow-release-success" msg))
    (update-contract (:id msg) #(assoc % :escrow-release "<success>"))))

(defmethod app-msg-handler :contract/escrow-release-failure
  [[_ msg]]
  (if (:error msg)
    (do (push-error "Error releasing the Escrow.")
        (utils/log* "Error in contract/escrow-release-failed" msg))
    (update-contract (:id msg) #(assoc % :escrow-release "<failure>"))))


(defmethod app-msg-handler :notification/create
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :notification/create" msg)
    (swap! (:notifications state/app) conj msg)))


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
  (utils/log* "Unhandled event: " (str event)))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (let [[old-state-map new-state-map] (have vector? ?data)]
    (if (:first-open? new-state-map)
      (network/run-initialization-callbacks)
      ;;(utils/log* "Channel socket state change: " new-state-map)
      )))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (utils/log* "Handshake completed...")))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data ?reply-fn event]}]
  (when (and (= ?data [:chsk/ws-ping]) ?reply-fn)
    (?reply-fn {:chsk/ws-ping event}))
  ;; (utils/log* "Push event from server: " (str ?data))
  (app-msg-handler ?data))

;;
;; Authentication
;;

(defn logout [] (aset js/window "location" "/"))

(defn authenticate [user-fbid user-name hashed-id friend-hashes]
  (sente/ajax-lite "/login"
                   {:method :post
                    :headers {:X-CSRF-Token (:csrf-token @network/chsk-state)}
                    :params {:hashed-id hashed-id}}
                   (fn [resp]
                     (let [logged (utils/json->clj (:?content resp))]
                       (if (:error logged)
                         (utils/log* "Login failed:" logged)
                         (do
                           (utils/log* "Login successful with ID:" (get logged "id"))
                           (network/register-init-callback!
                            (fn []
                              (reset! (:user-id state/app) (get logged "id"))
                              (reset! (:user-hash state/app) hashed-id)
                              (reset! (:user-fbid state/app) user-fbid)
                              (reset! (:user-name state/app) user-name)
                              (reset! (:friend-hashes state/app) friend-hashes)
                              (retrieve-app-data)
                              (retrieve-app-data-cycle-start!)))
                           (sente/chsk-reconnect! network/chsk)))))))

(defn- set-fake-facebooks-ids [{:keys [user-id hashed-id fb-id user-name friend-hashes] :as login}]
  (utils/log* "Connecting with fake data:" login)
  (authenticate fb-id user-name hashed-id friend-hashes))

(defn- set-facebook-ids [settable-error]
  (let [set-user! (fn [response]
                    (if (= (:status response) "connected")
                      (let [user-fbid-str (get-in response [:authResponse :userID])
                            user-fbid (js/Number (get-in response [:authResponse :userID]))
                            hashed-id (cljs-hash.goog/hash :sha1 user-fbid-str)]
                        (fb/api "/me/"
                                (fn [resp]
                                  (utils/log* "Connecting with Facebook userID: " user-fbid)
                                  (utils/log* "Hashed user: " hashed-id)
                                  (fb/api "/me/friends" {}
                                          (fn [{friends :data}]
                                            (let [friend-fbids (map :id friends)
                                                  hashed-friends (mapv #(cljs-hash.goog/hash :sha1 (str %)) friend-fbids)]
                                              (reset! (:friend-fbids state/app) friend-fbids)
                                              (reset! (:friend-hashes state/app) hashed-friends)
                                              (utils/log* "Friend IDs: " (str friend-fbids))
                                              (utils/log* "Hashed friends: " (str hashed-friends))
                                              (authenticate user-fbid (:name resp) hashed-id hashed-friends)))))))
                      (utils/log* "Not logged in: " (clj->js response))))]
    (try
      (fb/get-login-status
       (fn [response]
         (case (:status response)
           "connected"
           (set-user! response)
           (fb/login #(fb/get-login-status set-facebook-ids) {:scope "user_friends,public_profile"}))))
      (catch :default e (reset! settable-error (str e))))))

(defn login [settable-error]
  (if globals/hook-fake-id?_
    (set-fake-facebooks-ids (rand-nth common/fake-users))
    (set-facebook-ids settable-error)))
