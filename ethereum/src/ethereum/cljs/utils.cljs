(ns ethereum.utils
  (:require [cljs.nodejs :as nodejs]))

(defn node-slurp [path]
  (let [fs (nodejs/require "fs")]
    (.readFileSync fs path "utf8")))

(defn is [val]
  (if val
    (js/console.log "OK")
    (js/console.log "ERROR --")))
