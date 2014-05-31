(ns rook.engine
  (:require [rook.game :as g]
            [rook.cli :refer [cli-player cli-interface]]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]))

(defn start-game [game]
  (reset! game (g/new-game)))

(defn set-trump [game suit]
  (when @game
    (swap! game g/set-trump suit)))

(defn send-to-interfaces [interfaces f & args]
  (doseq [interface interfaces]
    (apply f interface args)))

(defn seat-player [game seat player]
  (when @game
    (swap! game update-in [:players seat] (constantly player))))

(defn game-loop [game-atom & interfaces]
  (send-to-interfaces interfaces print-initial-game-summary (deref game-atom))
  (loop []
    (let [game (deref game-atom)
          status (g/status game)
          position (:position status)
          player (get-in game [:players position])]
      (when-let [trick (:previous-trick status)]
        (let [summary (g/trick-summary game trick)]
          (send-to-interfaces interfaces print-trick-summary summary)))
      (if-let [card (get-card player status)]
        (do
          (swap! game-atom g/play card)
          (send-to-interfaces interfaces print-card-played player card)
          (recur))
        (send-to-interfaces interfaces print-score (g/score (deref game-atom)))))))

;; play as player 1
(defn cli-game []
  (let [game (atom nil)]
    (doto game
      (start-game)
      (seat-player 0 (cli-player))
      (seat-player 1 (intermediate-bot "John"))
      (seat-player 2 (intermediate-bot "Mary"))
      (seat-player 3 (intermediate-bot "Bob"))
      (set-trump :red))
    (game-loop game (cli-interface))))

(defn -main [& args]
  (cli-game))
