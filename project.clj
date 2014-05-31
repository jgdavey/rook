(defproject rook "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript  "0.0-2227"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [com.stuartsierra/component "0.2.1"]
                 [ring-server "0.3.1"]
                 [ring "1.2.2"]
                 [myguidingstar/clansi "1.3.0"]
                 [cheshire "5.3.1"]
                 [om "0.6.4"]
                 [com.ninjudd/eventual "0.3.1"]
                 [com.ninjudd/ring-async "0.2.3"]
                 [compojure "1.1.8"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src/clj"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs"]
              :compiler {
                ; :preamble ["react/react.min.js"]
                ; :externs ["react/externs/react.js"]
                :output-to "resources/public/dev.js"
                :output-dir "resources/public/out"
                :optimizations :none
                :source-map "resources/public/dev.js.map" }}]}

  :main ^:skip-aot rook.engine

  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.4"]]
                   :dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}})
