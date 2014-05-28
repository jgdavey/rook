(ns rook.engine
  (:require [rook.game :as g]
            [rook.cli :refer [cli-player print-score print-status print-trick-summary]]
            [rook.bots :refer [stupid-bot simple-bot]]
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
  (let [players [(cli-player) (simple-bot) (simple-bot) (stupid-bot)]]
    (loop []
      (let [status (status)
            position (:position status)
            player (get players position)
            my-turn? (= position 0)]
        (print-trick-summary (last-trick))
        (when my-turn?
          (print-status status))
        (if-let [card (get-card player status)]
          (do
            (play card)
            (recur))
          (print-score (score)))))))

(defn -main [& args]
  (cli-game))
