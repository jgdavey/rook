(ns user
  (:require [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [rook.system :refer (new-system)]
            [rook.engine :as e :refer (cli-game)]))

(defonce system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (new-system {:server {:port 8080}}))))

(defn start
  "Starts the current development system."
  []
  (alter-var-root #'system component/start))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system
                  (fn [s] (when s (component/stop s)))))

(defn go
  "Initializes the current development system and starts it running."
  []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
