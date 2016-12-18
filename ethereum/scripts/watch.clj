(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'ethereum.core
   :output-to "out/ethereum.js"
   :output-dir "out"})
