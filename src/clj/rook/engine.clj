(ns rook.engine
  (:require [clojure.core.async :as async :refer  [<! >!! chan go pub]]
            [rook.game :as g]
            [rook.cli :refer [cli-player]]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]))

(defn start-game [game]
  (reset! game (g/new-game)))

(defn seat-player [game seat player]
  (swap! game update-in [:players seat] (constantly player)))

(defn- bid-loop [state publish]
  (loop []
    (let [game (deref state)
          bid-status (g/bid-status game)
          active-bidders (:active-bidders bid-status)
          position (:position bid-status)
          player (get-in game [:players position])]
      (if (> (count active-bidders) 1)
        (let [bid (get-bid player bid-status)]
          (swap! state g/add-bid position bid)
          (publish :bid player bid)
          (recur))
        (do
          (swap! state g/award-bid-winner)
          (let [game (deref state)
                won (:winning-bid game)
                player (get-in game [:players (:seat won)])]
            (publish :bid-won player (:bid won))))))))

(defn- trick-loop [state publish]
  (loop []
    (let [game (deref state)
          status (g/status game)
          position (:position status)
          player (get-in game [:players position])]
      (when-let [trick (:previous-trick status)]
        (let [summary (g/trick-summary game trick)]
          (publish :trick-summary summary)))
      (publish [:player-status position] status)
      (when-let [card (get-card player status)]
        (swap! state g/play card)
        (publish :card-played player card)
        (recur)))))

(defn get-trump [state publish]
  (let [game @state
        seat (get-in game [:winning-bid :seat])
        hand (get-in game [:seats seat :dealt-hand])
        player (get-in game [:players seat])
        trump (choose-trump player hand)]
    (swap! state g/set-trump trump)
    (publish :trump-chosen trump)))

(defn get-kitty [state publish]
  (let [game @state
        winning-bid (:winning-bid game)
        seat (:seat winning-bid)
        hand (get-in game [:seats seat :dealt-hand])
        hand-and-kitty (set (concat hand (:kitty game)))
        player (get-in game [:players seat])]
    (publish [:hand-summary seat] hand-and-kitty)
    (when-let [new-kitty (choose-new-kitty player hand-and-kitty)]
      (swap! state g/choose-new-kitty seat new-kitty))))

(defn game-loop
  "state is an atom referencing the state of a game
  publish is a fn taking two arguments: the type of event and the payload"
  [state publish]
  (publish :summary @state)
  (doto state
    (bid-loop publish)
    (get-kitty publish)
    (get-trump publish)
    (trick-loop publish))
  (publish :score (g/score @state))
  :done)

;; play as player 1
(defn cli-game
  ([] (cli-game (atom nil)))
  ([game]
   (let [c (chan)
         pub-chan (pub c first)
         publish (fn [type & payload] (>!! c (concat [type] payload)))]
     (doto game
       (start-game)
       (seat-player 0 (cli-player pub-chan 0))
       (seat-player 1 (simple-bot "John"))
       (seat-player 2 (intermediate-bot "Mary"))
       (seat-player 3 (intermediate-bot "Bob")))
     (game-loop game publish))))

(defn -main [& args]
  (cli-game))
