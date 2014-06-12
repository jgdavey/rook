(ns rook.system
  (:require [com.stuartsierra.component :as component]
            [clojure.core.async :refer [chan]]
            [rook.repository :refer [new-repository]]
            [rook.router :refer [new-router]]
            [rook.server :refer [new-server]]))

(defrecord App [options server])

(defn app [config-options]
  (map->App {:options config-options}))

(defn new-system [config-options]
  (let [{:keys [server] :or {server {:port 8080}}} config-options
        {:keys [port]} server]
    (component/system-map
      :repository (new-repository)
      :router (component/using (new-router)
                                [:repository])
      :server (component/using (new-server port)
                                [:router])
      :app (component/using
             (app config-options)
             [:server]))))
