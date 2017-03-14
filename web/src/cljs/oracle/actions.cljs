(ns oracle.actions
  (:require [taoensso.encore :as encore :refer-macros (have have?)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            [goog.string :as gstring]
            [fb-sdk-cljs.core :as fb]
            ;; -----
            [oracle.common :as common]
            [oracle.state :as state]
            [oracle.utils :as utils]
            [oracle.network :as network]))

(declare event-msg-handler)

;;
;; Helpers
;;

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
  (swap! (:notifications state/app) conj {:title "Error" :message message}))

(defn logout [] (aset js/window "location" "/"))

(defn get-friends2 []
  (network/send!
   [:user/friends-of-friends {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (reset! (:friends2 state/app) (:friends2 resp))
       (do (push-error "There was an error with your login. Please try again.")
           (utils/log* "Error in handle-enter:" resp)))
     (utils/log* "Friends^2" (str @(:friends2 state/app))))))

(defn try-enter [user-fbid user-name hashed-id hashed-friends]
  (network/send!
   [:user/enter {:user-fbid user-fbid
                 :user-name user-name
                 :hashed-user hashed-id
                 :hashed-friends hashed-friends}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (do (utils/log* "Try Enter: " resp)
           (reset! (:user-id state/app) (:found-user resp)))
       (do (push-error "There was an error with your login. Please try again.")
           (utils/log* "Error in try-enter:" resp))))))

(defn- set-fake-facebooks-ids [hashed-id]
  (reset! (:user-hash state/app) hashed-id)
  #_(let [ffbids [10100642548250434 10106263879382352 10213129106885586 10210216755509404 145228535996960 145228535996960 145228535996960 145228535996960]]
    (reset! (:friend-fbids state/app) ffbids)
    #_(doseq [[f idx] (zipmap (take 8 f2bids) (range))]
      (fb/api (str "/" f "/picture")
              (fn [resp] (swap! (:friends2 state/app) conj {:id f
                                                            :name (str "Name " f)
                                                            :photo-url (get-in resp [:data :url])})))))
  (network/sente-register-init-callback! #(try-enter 1 "Alvatar" hashed-id ["TODO"]))
  (network/init-sente! event-msg-handler hashed-id))

(defn- set-facebook-ids [response]
  (if (= (:status response) "connected")
    (let [user-fbid-str (get-in response [:authResponse :userID])
          user-fbid (js/Number (get-in response [:authResponse :userID]))
          hashed-id (cljs-hash.goog/hash :sha1 user-fbid-str)]
      (fb/api "/me/"
              (fn [resp]
                (reset! (:user-name state/app) (:name resp))
                (reset! (:user-fbid state/app) user-fbid)
                (reset! (:user-hash state/app) hashed-id)
                (utils/log* "Connected with Facebook userID: " user-fbid)
                (utils/log* "Hashed user: " hashed-id)
                (fb/api "/me/friends" {}
                        (fn [{friends :data}]
                          (let [friend-fbids (map :id friends)
                                hashed-friends (mapv #(cljs-hash.goog/hash :sha1 (str %)) friend-fbids)]
                            (reset! (:friend-fbids state/app) friend-fbids)
                            (reset! (:friend-hashes state/app) hashed-friends)
                            (utils/log* "Friend IDs: " (str friend-fbids))
                            (utils/log* "Hashed friends: " (str hashed-friends))
                            (network/sente-register-init-callback! #(try-enter user-fbid (:name resp) hashed-id hashed-friends))
                            (network/init-sente! event-msg-handler hashed-id)))))))
    (utils/log* "Not logged in: " (clj->js response))))

(defn facebook-login [fb-error]
  (try
    (fb/get-login-status
     (fn [response]
       (case (:status response)
         "connected"
         (set-facebook-ids response)
         (fb/login #(fb/get-login-status set-facebook-ids) {:scope "user_friends,public_profile"}))))
    (catch :default e (reset! fb-error (str e)))))

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

(defn get-user-requests []
  (network/send!
   [:user/buy-requests {:user-id @(:user-id state/app)}] 5000
   (fn [resp]
     (if (and (sente/cb-success? resp) (= (:status resp) :ok))
       (when-let [requests (:buy-requests resp)]
         (utils/log* "Received requests" requests)
         (reset! (:buy-requests state/app) requests))
       (do (push-error "There was an error retrieving your previous buy requests. Please try again.")
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
       (do (push-error "There was an error retrieving your previous contracts. Please try again.")
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

(defn find-buy-request [id] (first (keep-indexed #(when (= (:id %2) id) %1) @(:buy-requests state/app))))

(defmethod app-msg-handler :buy-request/match
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/match" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests state/app) assoc-in [found-idx :seller-id] (:seller-id msg))
      (do (push-error "There was an error when matching the buy request. Please inform us of this event.")
          (utils/log* "Error in buy-request/match" msg)))))

(defmethod app-msg-handler :buy-request/timed-out
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/timed-out" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests state/app) assoc-in [found-idx :seller-id] nil)
      (do (push-error "There was an error when restarting the buy request. Please inform us of this event.")
          (utils/log* "Error in buy-request/timed-out" msg)))))

(defmethod app-msg-handler :buy-request/accepted
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/accepted" msg)
    (try (swap! (:buy-requests state/app) (fn [q] (remove #(= (:id msg)) q)))
         (catch :default e
           (push-error "There was an error when accepting the buy request. Please inform us of this event.")
           (utils/log* "Error in buy-request/accepted:" e)))))

(defmethod app-msg-handler :buy-request/declined
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :buy-request/declined" msg)
    (if-let [found-idx (find-buy-request (:id msg))]
      (swap! (:buy-requests state/app) assoc-in [found-idx :seller-id] nil)
      (do (push-error "There was an error when matching the buy request. Please inform us of this event.")
          (utils/log* "Error in buy-request/declined" msg)))))

(defmethod app-msg-handler :contract/create
  [[_ msg]]
  (if (:error msg)
    (utils/log* "Error in :contract/create" msg)
    (try (swap! (:contracts state/app) conj msg)
         ;; (when (= (:seller-id msg) @(:user-id state/app))
         ;;   (reset! (:display-contract state/app) (:id msg)))
         (catch :default e
           (do (push-error "There was an error when creating the contract. Please inform us of this event.")
               (utils/log* "Error in contract/create" msg))))))

(defmethod app-msg-handler :contract/update
  [[_ {:keys [stage status id amount]}]]
  (reset! (:contracts state/app)
          (for [c @(:contracts state/app)]
            (if (= (:id c) id) (merge c {:stage stage :status status}) c))))

(defmethod app-msg-handler :contract/escrow-funded
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error in funding the Escrow. Please inform us of this event.")
        (utils/log* "Error in contract/escrow-funded" msg))
    (update-contract (:id msg) #(assoc % :stage "waiting-transfer"))))

(defmethod app-msg-handler :contract/mark-transfer-received-ack
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error when marking the transfer as received. Please inform us of this event.")
        (utils/log* "Error in contract/mark-transfer-received-ack" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/holding-period
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error when starting the contract holding period. Please inform us of this event.")
          (utils/log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :transfer-received true))))

(defmethod app-msg-handler :contract/success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error setting the contract as successful. Please inform us of this event.")
        (utils/log* "Error in contract/holding-period" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-success"))))

(defmethod app-msg-handler :contract/broken
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error setting the contract as broken. Please inform us of this event.")
        (utils/log* "Error in contract/broken" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken"))))

(defmethod app-msg-handler :contract/escrow-insufficient
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
        (utils/log* "Error in contract/escrow-insufficient" msg))
    (update-contract (:id msg) #(assoc % :stage "contract-broken/escrow-insufficient"))))

(defmethod app-msg-handler :contract/escrow-release-success
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
        (utils/log* "Error in contract/escrow-release-success" msg))
    (update-contract (:id msg) #(assoc % :escrow-release "<success>"))))

(defmethod app-msg-handler :contract/escrow-release-failure
  [[_ msg]]
  (if (:error msg)
    (do (push-error "There was an error releasing the Escrow. Please inform us of this event.")
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
  (utils/log* "Push event from server: " (str ?data))
  (app-msg-handler ?data))