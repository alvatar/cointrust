(ns oracle.server
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as appenders]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.stacktrace :as trace]
            [ring.util.response :as response]
            [clojure.stacktrace :refer [print-stack-trace]]
            [compojure.core :refer [ANY GET PUT POST DELETE defroutes]]
            [compojure.route :refer [resources not-found]]
            [environ.core :refer [env]]
            [aleph [netty] [http]]
            [compojure.route :as route]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
            [taoensso.sente.packers.transit :as sente-transit]
            [rum.core :as rum]
            [cheshire.core :as json]
            ;; -----
            [oracle.actions :as actions]
            [oracle.events :as events]
            [oracle.tasks :as tasks]
            [oracle.bitcoin :as bitcoin]
            [oracle.worker :as worker]
            [oracle.database :as db]
            [oracle.currency :as currency]
            [oracle.html :as html])
  (:import (java.lang.Integer)
           (java.net InetSocketAddress)
           (java.io RandomAccessFile))
  (:gen-class))


;; Sente setup
(let [packer (sente-transit/get-transit-packer)
      {:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {:packer packer})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def connected-uids connected-uids))

;; (add-watch connected-uids :connected-uids
;;            (fn [_ _ old new]
;;              (when (not= old new)
;;                (log/debugf "****** Connected uids change: %s" new))))

(defn login-handler [req]
  (let [{:keys [session params]} req]
    (try
      (let [user-input (json/parse-string (:user params) true)
            {:keys [user-fbid user-name user-hash friend-hashes]} user-input]
        (if-let [user (db/user-insert! (BigDecimal. user-fbid) user-name user-hash friend-hashes)]
          {:status 200
           :session (assoc session :uid user-hash)
           :body (json/generate-string user)}
          {:status 200
           :body (json/generate-string {:error "login error"})}))
      (catch Exception e
        (pprint e)
        {:status 500}))))

(defroutes app
  (GET "/" _ (response/content-type
              ;; (response/resource-response "index.html" {:root "public"})
              (response/response (rum/render-html html/index))
              "text/html"))
  (resources "/" {:root "/public"})
  ;; Login
  (POST "/login" req (login-handler req))
  ;; Sente
  (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post req))
  (not-found "Woooot? Not found!"))

;;
;; Middleware
;;

(defn wrap-exceptions [app]
  "Ring wrapper providing exception capture"
  (let [wrap-error-page
        (fn [handler]
          (fn [req]
            (try (handler req)
                 (catch Exception e
                   (try (do (print-stack-trace e 60)
                            (println "== From request: ==")
                            (pprint req))
                        (catch Exception e2
                          (println "Exception trying to log exception?")))
                   {:status 500
                    :headers {"Content-Type" "text/plain"}
                    :body "500 Internal Error."}))))]
    ((case (:env env)
       ("production" "staging")
       wrap-error-page
       trace/wrap-stacktrace)
     app)))

;;
;; Server setup
;;

(defonce server (atom nil))

(defn start! [& [port ip]]
  (case (env :env)
    "dev" (log/set-level! :debug)
    "staging" (log/set-level! :debug)
    "production" (log/set-level! :debug))
  (actions/sente-router-start! ch-chsk)
  (events/init! chsk-send!)
  (currency/start-exchange-rates-updates!)
  (tasks/workers-start!)
  (reset! server
          (aleph.http/start-server
           (-> app
               (wrap-defaults (assoc-in (case (env :env)
                                          ("production" "staging") (assoc secure-site-defaults :proxy true)
                                          site-defaults)
                                        [:params :keywordize] true))
               wrap-exceptions
               wrap-gzip)
           {:port (Integer. (or port (env :port) 5000))
            :socket-address (if ip (new InetSocketAddress ip port))}))
  (bitcoin/system-start!)
  'startted)

(defn stop! []
  (when @server
    (tasks/workers-stop!)
    (currency/stop-exchange-rates-updates!)
    (.close @server)
    (reset! server nil)
    (bitcoin/system-stop!)
    'stopped))

(defn -main [& [port ip]]
  (start! port ip)
  (aleph.netty/wait-for-close @server))
