(defproject figwheel4node "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.107"]]
  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.8"]]
  :clean-targets ^{:protect false} ["target"]
  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.8.2"]
                   [com.cemerick/piggieback "0.2.1"]
                   [figwheel-sidecar "0.5.8"]
                   [org.clojure/tools.nrepl "0.2.11"]]
    :plugins [[lein-figwheel "0.5.8"]]
    :source-paths ["env/dev"]
    :cljsbuild {:builds [{:id "dev"
                          :source-paths ["src/ethereum"]
                          :figwheel {:on-jsload "ethereum.core/mount-root"}
                          :compiler {:main ethereum.core
                                     :output-to "target/server_out/ethereum_with_figwheel.js"
                                     :output-dir "target/server_out"
                                     :target :nodejs
                                     :optimizations :none
                                     :source-map-timestamp true
                                     :source-map true}}]}}

   ;; :uberjar {:hooks [leiningen.cljsbuild]
   ;;               :omit-source true
   ;;               :aot :all
   ;;               :main emojillionaire.core
   ;;               :cljsbuild {:builds {:app {:id "uberjar"
   ;;                                          :source-paths ["src/cljs"]
   ;;                                          :compiler {:main clojurescript-ethereum-example.core
   ;;                                                     :output-to "resources/public/js/compiled/app.js"
   ;;                                                     :optimizations :advanced
   ;;                                                     :closure-defines {goog.DEBUG false}
   ;;                                                     :pretty-print true
   ;;                                                     :pseudo-names true}}}}}
   }
  :figwheel {})
