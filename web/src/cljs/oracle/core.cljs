(ns oracle.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs-react-material-ui.core :refer [adapt-rum-class]])
  (:require [cljs.core.async :as async :refer (<! >! put! take! chan)]
            [rum.core :as rum]
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

(defonce app_ (rum/mount (components/app) (js/document.getElementById "app")))

(defonce network_ (network/init-sente! actions/event-msg-handler))
