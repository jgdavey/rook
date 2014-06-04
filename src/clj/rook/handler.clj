(ns rook.handler
  (:require [rook.game :as g]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]))

(defrecord Player [name]
  IPlayer
  (get-card [this status])
  (display-name [this] (:name this)))
