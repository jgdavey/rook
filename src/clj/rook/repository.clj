(ns rook.repository
  (:require [com.stuartsierra.component :as component])
  (:import [java.util UUID]))

(defprotocol IGameRepository
  (add-game [this game])
  (find-game [this id]))

(defn uuid []
  (str (UUID/randomUUID)))

(defrecord Repository [games]
  component/Lifecycle

  (start [component]
    (assoc component :games (atom {})))
  (stop [component]
    (dissoc component :games))

  IGameRepository
  (add-game [repository game]
    (let [id (uuid)]
      (when (swap! (:games repository) assoc id game)
        id)))
  (find-game [repository uuid]
    (get @(:games repository) uuid)))

(defn new-repository []
  (->Repository nil))
