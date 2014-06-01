(ns rook.protocols)

(defprotocol IPlayer
  (display-name [this])
  (get-card [this status])
  (get-bid [this bid-status])
  (choose-new-kitty [this hand-and-kitty])
  (choose-trump [this hand]))
