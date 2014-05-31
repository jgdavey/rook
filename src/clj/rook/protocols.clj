(ns rook.protocols)

(defprotocol IPlayer
  (get-card [this status])
  (display-name [this]))

(defprotocol IUserInterface
  (print-initial-game-summary [this game-summary])
  (print-score [this score])
  (print-trick-summary [this trick-summary])
  (print-card-played [this player card]))
