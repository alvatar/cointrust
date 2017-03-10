(ns oracle.html
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [environ.core :refer [env]]))

(def index
  [:html
   [:head
    [:title "Cointrust"]
    [:meta {:charset "UTF-8"}]
    [:meta {:name "viewport" :content "user-scalable=no, width=device-width, initial-scale=1.0"}]
    [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
    [:link {:href "css/style.css" :rel "stylesheet" :type "text/css"}]]
   [:body
    [:div#app]
    [:script {:src "js/compiled/oracle.js" :type "text/javascript"}]]])
