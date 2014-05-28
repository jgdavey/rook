(ns rook.system
  (:require [com.stuartsierra.component :as component]
            [rook.server :refer [new-server]]))

(defrecord App [options server])

(defn app [config-options]
  (map->App {:options config-options}))

(defn new-system [config-options]
  (let [{:keys [server] :or {server {:port 8080}}} config-options
        {:keys [port]} server]
    (component/system-map
      :server (new-server port)
      :app (component/using
             (app config-options)
             [:server]))))
