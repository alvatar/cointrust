(ns oracle.network
  (:require [taoensso.sente :as sente :refer (cb-success?)]
            [taoensso.sente.packers.transit :as sente-transit]
            ;; -----
            [oracle.utils :as utils]
            [oracle.state :as state]))

;;
;; Setup
;;

(enable-console-print!)

(defonce router_ (atom nil))
(defonce sente-callback-registry (atom []))
(defonce sente-init-callbacks-ran? (atom false))

(defn register-init-callback! [callback]
  (swap! sente-callback-registry conj callback))

(defn run-initialization-callbacks []
  (when-not @sente-init-callbacks-ran?
    (doseq [cb @sente-callback-registry] (cb))
    (reset! sente-init-callbacks-ran? true)))

(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! [ch-chsk event-msg-handler]
  ;;(stop-router!)
  (utils/log* "Initializing Sente client router...")
  (reset! router_ (sente/start-client-chsk-router! ch-chsk event-msg-handler)))

(def sente-reconnector (atom nil))

(defn init-sente! [event-msg-handler]
  (utils/log* "Initializing Sente...")
  (let [packer (sente-transit/get-transit-packer)
        {:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket! "/chsk" {:type :auto :packer packer})]
    (def chsk chsk)
    (def ch-chsk ch-recv)             ; ChannelSocket's receive channel
    (def send! send-fn)               ; ChannelSocket's send API fn
    (def chsk-state state)            ; Watchable, read-only atom
    (start-router! ch-chsk event-msg-handler)
    (add-watch chsk-state :chsk-state-reconnect
               (fn [_1 _2 _3 _4]
                 (cond (and (:ever-opened? _4) (not (:open? _4)))
                       (swap! sente-reconnector
                              (fn [v] (or v (js/setInterval (fn []
                                                              (when (zero? (swap! (:seconds-until-next-reconnect state/app)
                                                                                  #(if (or (not %) (zero? %)) 10 (dec %))))
                                                                (sente/chsk-reconnect! chsk)))
                                                            1000))))
                       (:open? _4)
                       (do (swap! sente-reconnector #(when % (js/clearInterval %) nil))
                           (reset! (:seconds-until-next-reconnect state/app) nil)))))))
