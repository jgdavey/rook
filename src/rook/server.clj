(ns rook.server
  (:require [com.stuartsierra.component :as component]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.adapter.jetty-async :refer [run-jetty-async]]))

(defroutes routes
  (GET "/" []  "<h1>Hello</h1>"))

(defrecord Server [port instance irc-channel]
  component/Lifecycle

  (start [component]
    (if instance
      component
      (do
        (println ";; Starting jetty server")
        (let [instance (run-jetty-async routes {:port port :join? false})]
          (assoc component :instance instance)))))

  (stop [component]
    (if (not instance)
      component
      (do
        (println ";; Stopping jetty server")
        (.stop instance)
        (dissoc component :instance)))))

(defn new-server
  [port]
  (map->Server {:port port}))
