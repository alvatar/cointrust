(ns user
  (:require [figwheel-sidecar.repl-api]
            [ethereum.core]
            [ring.middleware.reload :refer [wrap-reload]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
