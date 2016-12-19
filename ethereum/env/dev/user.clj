(ns user
  (:require [figwheel-sidecar.repl-api]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn run []
  (figwheel-sidecar.repl-api/start-figwheel! (figwheel-sidecar.config/fetch-config))
  (figwheel-sidecar.repl-api/cljs-repl)
  (in-ns 'ethereum.core))
