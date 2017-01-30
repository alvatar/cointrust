(ns oracle.server
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
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
            ;; -----
            [oracle.actions :as actions]
            [oracle.events :as events]
            [oracle.tasks :as tasks]
            [oracle.bitcoin :as bitcoin])
  (:import (java.lang.Integer)
           (java.net InetSocketAddress)
           (java.io RandomAccessFile))
  (:gen-class))


;; Sente setup
(let [packer (sente-transit/get-transit-packer)
      {:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {:packer packer
                                                     :user-id-fn (fn [ring-req] (:client-id ring-req))})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def connected-uids connected-uids))

(defroutes app
  (GET "/" _ (response/content-type
              (response/resource-response "index.html" {:root "public"})
              "text/html"))
  (resources "/" {:root "/public"})
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
    ((if (or (env :production)
             (env :staging))
       wrap-error-page
       trace/wrap-stacktrace)
     app)))

;;
;; Server setup
;;

(defonce server (atom nil))

(onelog.core/set-debug!)

(defn start! [& [port ip]]
  (log/set-level! :debug)
  (actions/sente-router-start! ch-chsk)
  (events/init! chsk-send!)
  (reset! server
          (aleph.http/start-server
           (-> app
               (wrap-defaults (assoc-in (if (env :production) secure-site-defaults site-defaults)
                                        [:params :keywordize] true))
               wrap-exceptions
               wrap-gzip)
           {:port (Integer. (or port (env :port) 5000))
            :socket-address (if ip (new InetSocketAddress ip port))})))

(defn stop! []
  (when @server
    (.close @server)
    (reset! server nil)
    'stopped))

(defn -main [& [port ip]]
  (start! port ip)
  (aleph.netty/wait-for-close @server))
