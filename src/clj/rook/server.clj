(ns rook.server
  (:require [com.stuartsierra.component :as component]
            [ring.util.response :refer [file-response]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.adapter.jetty-async :refer [run-jetty-async]]))

(defn index []
  (file-response "public/index.html" {:root "resources"}))

(defroutes routes
  (GET "/" [] (index))
  (route/files "/" {:root "resources/public"}))

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
