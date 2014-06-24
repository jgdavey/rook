(ns rook.seat
  (:require [clojure.core.async :refer [go >! <!]]))

(defprotocol IGoable
  (in [this])
  (out [this])
  (dispatch [this message]))

(defprotocol IConnected
  (connected? [this]))

(defprotocol ISeat
  (display-name [this]))

(defn go-seat [goable]
  (go (loop []
        (when-some [v (<! (in goable))]
          (when-some [response (dispatch goable v)]
            (>! (out goable) response))
          (recur)))))
