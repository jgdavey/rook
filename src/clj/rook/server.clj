(ns rook.server
  (:require [com.stuartsierra.component :as component]
            [rook.router :as router]
            [org.httpkit.server :refer [run-server]]))

(defrecord Server [port instance router]
  component/Lifecycle

  (start [component]
    (if instance
      component
      (do
        (println ";; Starting server")
        (let [instance (run-server (:handler router) {:port port})]
          (assoc component :instance instance)))))

  (stop [component]
    (if (not instance)
      component
      (do
        (println ";; Stopping server")
        (instance :timeout 100)
        (dissoc component :instance)))))

(defn new-server
  [port]
  (map->Server {:port port}))
