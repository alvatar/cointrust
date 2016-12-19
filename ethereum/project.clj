(defproject ethereum "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.8"]]
  :clean-targets ^{:protect false} ["target"]
  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.8.3"]
                   [com.cemerick/piggieback "0.2.1"]
                   [figwheel-sidecar "0.5.8"]
                   [org.clojure/tools.nrepl "0.2.12"]]
    :plugins [[lein-figwheel "0.5.8"]
              [lein-ancient "0.6.10"]]
    :source-paths ["env/dev"]
    :cljsbuild {:builds [{:id "dev"
                          :source-paths ["src/ethereum"]
                          :figwheel {:on-jsload "ethereum.core/mount-root"}
                          :compiler {:main ethereum.core
                                     :output-to "target/dev/ethereum.js"
                                     :output-dir "target/dev/"
                                     :target :nodejs
                                     :optimizations :none
                                     :source-map-timestamp true
                                     :source-map true}}]}}
   :deploy {:hooks [leiningen.cljsbuild]
            :omit-source true
            :aot :all
            :main ethereum.core
            :cljsbuild {:builds {:app {:id "uberjar"
                                       :source-paths ["src/deploy/"]
                                       :compiler {:main ethereum.core
                                                  :output-to "target/deploy/ethereum.js"
                                                  ;; Try advanced, but see https://github.com/bhauman/lein-figwheel/wiki/Node.js-development-with-figwheel
                                                  :optimizations :simple
                                                  :closure-defines {goog.DEBUG false}
                                                  :pretty-print true
                                                  :pseudo-names true}}}}}}
  :figwheel {})
