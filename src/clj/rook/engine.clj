(ns rook.engine
  (:require [rook.game :as g]
            [rook.cli :refer [cli-player print-card-played print-score print-status print-trick-summary]]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]
            ))

(defonce game (atom nil))

(defn start-game []
  (reset! game (g/new-game)))

(defmacro defdelegate [fname bindings]
  (let [rfn (get (ns-publics 'rook.game) fname)]
    `(defn ~fname ~bindings
       (when (deref game)
         (~rfn (deref game) ~@bindings)))))

(defdelegate next-player [])
(defdelegate unplayed-cards [player])
(defdelegate beginning-of-game? [])
(defdelegate score [])
(defdelegate status [])
(defdelegate trick-summary [trick])

(defn trick-in-play []
  (when @game
    (g/trick-in-play (:tricks @game))))

(defn last-trick []
  (when @game
    (peek (:tricks @game))))

(defn set-trump [suit]
  (when @game
    (swap! game g/set-trump suit)))

(defn play [card]
  (when @game
    (swap! game g/play card)))

;; play as player 1
(defn cli-game []
  (start-game)
  (set-trump :red)
  (let [players [(cli-player)
                 (intermediate-bot "John")
                 (intermediate-bot "Mary")
                 (intermediate-bot "Bob")]]
    (loop []
      (let [status (status)
            position (:position status)
            player (get players position)]
        (when-let [trick (:previous-trick status)]
          (let [summary (trick-summary trick)]
            (print-trick-summary summary)))
        (if-let [card (get-card player status)]
          (do
            (play card)
            (print-card-played (display-name player) card)
            (recur))
          (print-score (score)))))))

(defn -main [& args]
  (cli-game))
