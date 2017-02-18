(defproject oracle "0.1.0-SNAPSHOT"
  :description "Cointrust Web Service"
  :url "http://cointrust.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.456"]
                 [com.cognitect/transit-clj "0.8.297"]
                 [com.cognitect/transit-cljs "0.8.239"]
                 [environ "1.1.0"]
                 [ring "1.5.1"]
                 [ring/ring-defaults "0.2.3"]
                 [bk/ring-gzip "0.2.1"]
                 [aleph "0.4.2-alpha12"]
                 [manifold "0.1.5"]
                 [byte-streams "0.2.2"]
                 [compojure "1.5.2"]
                 [com.taoensso/nippy "2.13.0"]
                 [com.taoensso/sente "1.11.0"]
                 [com.taoensso/encore "2.89.0"]
                 [com.taoensso/timbre "4.8.0"]
                 ;; Database
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4.1212"]
                 [com.taoensso/carmine "2.15.1" :exclusions [com.taoensso/encore]]
                 ;; Bitcoin
                 [org.bitcoinj/bitcoinj-core "0.14.4" :exclusions [com.google.protobuf/protobuf-java
                                                                   com.google.guava/guava
                                                                   com.google.code.findbugs/jsr305]]
                 ;; Misc
                 [crypto-random "1.2.0"]
                 [cheshire "5.7.0"]
                 [clj-time "0.13.0"]
                 [com.cemerick/url "0.1.1"]
                 ;; Cljs
                 [rum "0.10.8" :exclusions [cljsjs/react cljsjs/react-dom]]
                 [cljs-react-material-ui "0.2.37"]
                 [cljsjs/rc-slider "4.0.1-0" :exclusions [cljsjs/react cljsjs/react-dom]]
                 [netpyoung/fb-sdk-cljs "0.1.2" :exclusions [com.google.guava/guava]]
                 [cljs-hash "0.0.2"]
                 [datascript "0.15.5"]
                 [camel-snake-kebab "0.4.0"]]

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-environ "1.0.3"]
            [lein-less "1.7.5"]]

  :min-lein-version "2.6.1"

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :test-paths ["test/clj" "test/cljc"]

  :clean-targets ^{:protect false} [:target-path :compile-path "resources/public/js"]

  :uberjar-name "oracle.jar"

  ;; Use `lein run` if you just want to start a HTTP server, without figwheel
  :main oracle.server

  ;; nREPL by default starts in the :main namespace, we want to start in `user`
  ;; because that's where our development helper functions like (run) and
  ;; (browser-repl) live.
  :repl-options {:init-ns user}

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/cljs" "src/cljc"]
                :figwheel {:websocket-host :js-client-host}
                ;; Alternatively, you can configure a function to run every time figwheel reloads.
                ;; :figwheel {:on-jsload "oracle.core/on-figwheel-reload"}
                :compiler {:main oracle.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/oracle.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true
                           :closure-defines {oracle.core/*is-dev* true}}}
               {:id "test"
                :source-paths ["src/cljs" "test/cljs" "src/cljc" "test/cljc"]
                :compiler {:output-to "resources/public/js/compiled/testable.js"
                           :main oracle.test-runner
                           :optimizations :none
                           :closure-defines {oracle.core/*is-dev* true}}}
               {:id "min"
                :source-paths ["src/cljs" "src/cljc"]
                :jar true
                :compiler {:main oracle.core
                           :output-to "resources/public/js/compiled/oracle.js"
                           :output-dir "target"
                           :source-map-timestamp true
                           :optimizations :simple
                           :pretty-print false}}]}
  ;; When running figwheel from nREPL, figwheel will read this configuration
  ;; stanza, but it will read it without passing through leiningen's profile
  ;; merging. So don't put a :figwheel section under the :dev profile, it will
  ;; not be picked up, instead configure figwheel here on the top level.
  :figwheel {;; :http-server-root "public"       ;; serve static assets from resources/public/
             ;; :server-port 3449                ;; default
             ;; :server-ip "127.0.0.1"           ;; default
             :server-ip "0.0.0.0"
             :css-dirs ["resources/public/css"]  ;; watch and update CSS
             ;; Instead of booting a separate server on its own port, we embed
             ;; the server ring handler inside figwheel's http-kit server, so
             ;; assets and API endpoints can all be accessed on the same host
             ;; and port. If you prefer a separate server process then take this
             ;; out and start the server with `lein run`.
             :ring-handler user/http-handler
             ;; Start an nREPL server into the running figwheel process. We
             ;; don't do this, instead we do the opposite, running figwheel from
             ;; an nREPL process, see
             ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
             ;; :nrepl-port 7888
             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"
             :server-logfile "log/figwheel.log"}

  :doo {:build "test"}

  :less {:source-paths ["src/less"]
         :target-path "resources/public/css"}

  :profiles {:dev
             {:dependencies [[figwheel "0.5.9" :exclusions [com.google.guava/guava]]
                             [figwheel-sidecar "0.5.9" :exclusions [com.google.guava/guava]]
                             [com.cemerick/piggieback "0.2.1"]
                             [org.clojure/tools.nrepl "0.2.12"]]
              :plugins [[lein-figwheel "0.5.9"]
                        [lein-doo "0.1.6"]
                        [lein-ancient "0.6.10"]]
              :source-paths ["dev"]
              :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
              :env {:env "dev"}}
             :uberjar
             {:source-paths ^:replace ["src/clj" "src/cljc"]
              :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
              :hooks [leiningen.less]
              :omit-source true
              :aot :all
              :env {:env "uberjar"}}})
