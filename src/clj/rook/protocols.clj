(ns rook.protocols)

(defprotocol IPlayer
  (get-card [this status])
  (display-name [this])
  (get-bid [this bid-status]))
