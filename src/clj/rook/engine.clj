(ns rook.engine
  (:require [clojure.core.async :as async :refer  [<! >!! chan go pub]]
            [rook.game :as g]
            [rook.cli :refer [cli-player]]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]))

(defn start-game [game]
  (reset! game (g/new-game)))

(defn set-trump [game suit]
  (swap! game g/set-trump suit))

(defn seat-player [game seat player]
  (swap! game update-in [:players seat] (constantly player)))

(defn game-loop
  "game-atom is an atom referencing the state of a game
  publish is a fn taking two arguments: the type of event and the payload"
  [game-atom publish]
  (publish :summary (deref game-atom))
  (loop []
    (let [game (deref game-atom)
          status (g/status game)
          position (:position status)
          player (get-in game [:players position])]
      (when-let [trick (:previous-trick status)]
        (let [summary (g/trick-summary game trick)]
          (publish :trick-summary summary)))
      (publish [:player-status position] status)
      (if-let [card (get-card player status)]
        (do
          (swap! game-atom g/play card)
          (publish :card-played player card)
          (recur))
        (publish :score (g/score (deref game-atom)))))))

;; play as player 1
(defn cli-game []
  (let [game (atom nil)
        c (chan)
        pub-chan (pub c first)
        publish (fn [type & payload] (>!! c (concat [type] payload)))]
    (doto game
      (start-game)
      (set-trump :red)
      (seat-player 0 (cli-player pub-chan 0))
      (seat-player 1 (intermediate-bot "John"))
      (seat-player 2 (intermediate-bot "Mary"))
      (seat-player 3 (intermediate-bot "Bob")))
    (game-loop game publish)))

(defn -main [& args]
  (cli-game))
