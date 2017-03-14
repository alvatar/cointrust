(ns oracle.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs-react-material-ui.core :refer [adapt-rum-class]])
  (:require [cljs.core.async :as async :refer (<! >! put! take! chan)]
            [rum.core :as rum]
            [cljs-hash.goog :as gh]
            [fb-sdk-cljs.core :as fb]
            ;; -----
            [oracle.globals :as globals]
            [oracle.state :as state]
            [oracle.utils :as utils]
            [oracle.components :as components]
            [oracle.actions :as actions]
            [oracle.network :as network]))

;;
;; Init
;;

(js/console.log (str "Frontend environment is: " globals/*env*))

(try
  (fb/load-sdk (fn []
                 (js/console.log "Facebook lib loaded")
                 (fb/init {:appId (case globals/*env*
                                    "production" "1131377006981108"
                                    "staging" "1382322701791260"
                                    ("test" "dev") "1214574721994669")
                           :status true
                           :cookies false
                           :xfbml true
                           :version "v2.8"})))
  (catch :default e (js/console.log e)))

(rum/mount (components/app) (js/document.getElementById "app"))

;;
;; Watchers
;;

;; Run when we change the User ID
(add-watch (:user-id state/app) :got-user-id
           (fn [_1 _2 _3 _4]
             (when _4
               (actions/get-server-time)
               (js/setInterval actions/get-server-time 20000)
               (js/setInterval #(swap! (:server-time state/app) (partial + 1000)) 1000)
               (actions/get-exchange-rates)
               (js/setInterval #(let [a (:seconds-since-last-exchange-rates-refresh state/app)]
                                  (if (< @a globals/exchange-rates-refresh-interval)
                                    (swap! a inc)
                                    (do (actions/get-exchange-rates) (reset! a 0))))
                               1000)
               (actions/get-friends2)
               (actions/get-active-sell-offer)
               (actions/get-sell-offer-matches)
               (actions/get-user-requests)
               (actions/get-user-contracts)
               (actions/get-user-pending-notifications))))

;; TODO: put in actions

(add-watch (:friends2 state/app) :got-friends2
           (fn [_1 _2 _3 _4]
             (doseq [[f idx] (zipmap (take 8 @(:friends2 state/app)) (range))]
               (fb/api (str "/" (:fb-id f) "/picture")
                       #(if-let [photo-url (get-in % [:data :url])]
                          (swap! (:friends2 state/app) assoc-in [idx :photo-url] photo-url)
                          (utils/log* %))))
             (remove-watch (:friends2 state/app) :got-friends2)))

(defn get-photo-for! [obj type role]
  (fb/api (str "/" ((keyword (str (name role) "-fb-id")) obj) "/picture")
          (fn [resp]
            (if-let [photo-url (get-in resp [:data :url])]
              ((case type
                 :sell-offer actions/update-sell-offer
                 :buy-request actions/update-buy-request
                 :contract actions/update-contract)
               (:id obj) #(assoc % (keyword (str (name role) "-photo")) photo-url))
              (utils/log* resp)))))

(add-watch (:sell-offer-matches state/app) :fetch-sell-offer-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when (and (:buyer-id c) (not (:buyer-photo c)))
                 (get-photo-for! c :sell-offer :buyer)))))

(add-watch (:buy-requests state/app) :fetch-buy-requests-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when (and (:buyer-id c) (not (:buyer-photo c)))
                 (get-photo-for! c :buy-request :buyer)))))

(add-watch (:contracts state/app) :fetch-contract-photos
           (fn [_1 _2 _3 _4]
             (doseq [c @_2]
               (when-not (:seller-photo c) (get-photo-for! c :contract :seller))
               (when-not (:buyer-photo c) (get-photo-for! c :contract :buyer)))))
