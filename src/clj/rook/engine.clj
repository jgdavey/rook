(ns rook.engine
  (:require [clojure.core.async :as async :refer  [<! >! put! <!! >!! go timeout]]
            [clojure.stacktrace]
            [rook.game :as g]
            [rook.cli :refer [cli-player]]
            [rook.bots :refer [bot]]
            [rook.seat :refer :all]))

(defn publish [game topic message]
  (let [t (keyword "rook" (name topic))
        players (:players @game)]
    (doseq [player players]
      (when (satisfies? IGoable player)
        (put! (in player) [t message])))))

(defn publish-go [game topic message]
  (let [t (keyword "rook" (name topic))
        players (:players @game)]
    (async/merge
      (doseq [player (filter #(satisfies? IGoable %) players)]
        (go (>! (in player) [t message]))))))

(defn start-game [game]
  (reset! game (g/new-game)))

(defn seat-player [game seat player]
  (swap! game update-in [:players seat] (constantly player)))

(defn position-of-id [m id]
  (let [indexed (map-indexed vector m)]
    (some (fn [[i p]] (when (= id (:id p)) i)) indexed)))

(defn position-of-player-id [state id]
  (position-of-id (:players @state) id))

(defn find-player-with-id [state id]
  (some #(when (= id (:id %)) %) (:players @state)))

(defn- connect-loop [state]
  (go (loop [i 1]
        (when (< i 11)
          (when (some nil? (:players @state))
            (when (= i 1)
              (print "\nWaiting for all players to connect"))
            (print ".") (flush)
            (<! (timeout (* 100 i)))
            (recur (inc i)))))))

(defn- bid-loop [state]
  (go
    (loop []
      (<! (connect-loop state))
      (let [game (deref state)
            bid-status (g/bid-status game)
            active-bidders (:active-bidders bid-status)
            position (:position bid-status)
            player (get-in game [:players position])]
        (if (> (count active-bidders) 1)
          (let [_ (>! (in player) [:rook/get-bid bid-status])
                bid (<! (out player))]
            (when bid
              (swap! state g/add-bid position bid)
              (publish state :bid [position bid]))
            (recur))
          (do
            (swap! state g/award-bid-winner)
            (let [game (deref state)
                  won (:winning-bid game)
                  player (get-in game [:players (:seat won)])]
              (publish state :bid-won [(:seat won) (:bid won)]))))))))

(defn- trick-loop [state]
  (go
    (loop []
      (<! (connect-loop state))
      (when-not (g/game-over? @state)
        (let [game (deref state)
              status (g/status game)
              position (:position status)
              player (get-in game [:players position])]
          (when-let [trick (:previous-trick status)]
            (let [summary (g/trick-summary game trick)]
              (publish state :trick-summary summary)))
          (>! (in player) [:rook/get-card status])
          (if-let [card (<! (out player))]
            (when-not (= :quit card)
              (swap! state g/play card)
              (publish state :card-played [position card])
              (recur))
            (recur)))))))

(defn get-trump [state]
  (go
    (let [game @state
          seat (get-in game [:winning-bid :seat])
          hand (get-in game [:seats seat :dealt-hand])
          player (get-in game [:players seat])]
      (>! (in player) [:rook/choose-trump hand])
      (when-let [trump (<! (out player))]
        (swap! state g/set-trump trump)
        (publish state :trump-chosen trump)))))

(defn get-kitty [state]
  (go
    (let [game @state
          winning-bid (:winning-bid game)
          seat (:seat winning-bid)
          hand (get-in game [:seats seat :dealt-hand])
          hand-and-kitty (set (concat hand (:kitty game)))
          player (get-in game [:players seat])]
      (>! (in player) [:rook/choose-kitty hand-and-kitty])
      (when-let [new-kitty (<! (out player))]
        (swap! state g/choose-new-kitty seat new-kitty)))))

(defn players-summary [game]
  (mapv (fn [s p]
          {:count (count (g/unplayed-cards game s))
           :name (display-name p)})
        (range)
        (:players game)))

(defn go-player-summary [state position player]
  (let [status (g/player-summary @state position)]
    (go (>! (in player) [:rook/status (assoc status :players (players-summary @state))]))))

(defn game-start [state]
  (async/merge (map-indexed (partial go-player-summary state) (:players @state))))

(defn game-over [state]
  (publish-go state :score (g/score @state)))

(defn game-loop
  "state is an atom referencing the state of a game"
  [state]
  (go
    (<! (connect-loop state))
    (<! (game-start state))
    (<! (bid-loop state))
    (<! (get-kitty state))
    (<! (get-trump state))
    (<! (trick-loop state))
    (<! (game-over state))))

;; play as player 1
(defn cli-game
  ([] (cli-game (atom nil)))
  ([game]
   (doto game
     (start-game)
     (seat-player 0 (cli-player))
     (seat-player 1 (bot :name "Homer" :strategy :simple))
     (seat-player 2 (bot :name "Marge" :strategy :intermediate))
     (seat-player 3 (bot :name "Lisa"  :strategy :intermediate)))
   (<!! (game-loop game))
   :done))

(defn setup-web-game [game]
  (doto game
    (start-game)
    (seat-player 0 nil)
    (seat-player 1 (bot :name "Homer" :strategy :simple))
    (seat-player 2 (bot :name "Marge" :strategy :intermediate))
    (seat-player 3 (bot :name "Lisa"  :strategy :intermediate))))

(defn disconnect-player [game player-id]
  (if-let [loc (position-of-player-id game player-id)]
    (seat-player game loc nil)))

(defn connect-player [game player]
  (let [loc (position-of-player-id game (:id player))
        position (or loc 0)]
    (seat-player game position player)
    (<!! (go-player-summary game position player))))

(defn -main [& args]
  (cli-game))
