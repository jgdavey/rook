(ns rook.engine
  (:require [clojure.core.async :as async :refer  [<! >!! chan go pub close!]]
            [rook.game :as g]
            [rook.cli :refer [cli-player]]
            [rook.bots :refer [intermediate-bot stupid-bot simple-bot]]
            [rook.protocols :refer :all]))

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
  (loop [i 0]
    (if (> i 10)
      (.interrupt (Thread/currentThread))
      (when (some nil? (:players @state))
        (println "Waiting for all players to connect")
        (Thread/sleep (* i 200))
        (recur (inc i))))))

(defn- bid-loop [state publish]
  (loop []
    (connect-loop state)
    (let [game (deref state)
          bid-status (g/bid-status game)
          active-bidders (:active-bidders bid-status)
          position (:position bid-status)
          player (get-in game [:players position])]
      (if (> (count active-bidders) 1)
        (let [bid (get-bid player bid-status)]
          (swap! state g/add-bid position bid)
          (publish :bid position bid)
          (recur))
        (do
          (swap! state g/award-bid-winner)
          (let [game (deref state)
                won (:winning-bid game)
                player (get-in game [:players (:seat won)])]
            (publish :bid-won position (:bid won))))))))

(defn- trick-loop [state publish]
  (loop []
    (connect-loop state)
    (let [game (deref state)
          status (g/status game)
          position (:position status)
          player (get-in game [:players position])]
      (when-let [trick (:previous-trick status)]
        (let [summary (g/trick-summary game trick)]
          (publish :trick-summary summary)))
      (let [card (get-card player status)]
        (when card
          (when-not (identical? :timeout card)
            (swap! state g/play card)
            (publish :card-played position card))
          (recur))))))

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
    (when-let [new-kitty (choose-new-kitty player hand-and-kitty)]
      (swap! state g/choose-new-kitty seat new-kitty))))

(defn game-loop
  "state is an atom referencing the state of a game
  publish is a fn taking two arguments: the type of event and the payload"
  [state publish]
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
       (seat-player 3 (intermediate-bot "Bob"))
       (swap! assoc :pub-chan pub-chan))
     (game-loop game publish))))

(defn setup-web-game [game]
  (let [c (chan)
        pub-chan (pub c (constantly :all))
        publish (fn [type & payload] (>!! c (concat [type] payload)))]
    (doto game
      (start-game)
      (seat-player 0 nil)
      (seat-player 1 (simple-bot "John"))
      (seat-player 2 (intermediate-bot "Mary"))
      (seat-player 3 (intermediate-bot "Bob"))
      (swap! assoc :pub-chan pub-chan)
      (swap! assoc :chan c)
      (swap! assoc :publish-fn publish))))

(defn web-game-loop [game]
  (try
    (connect-loop game)
    (game-loop game (get-in @game [:publish-fn]))
    (close! (:chan @game))
    (catch Throwable e
      (println e)
      (.printStackTrace e *out*))))

(defn disconnect-player [game player-id]
  (if-let [loc (position-of-player-id game player-id)]
    (seat-player game loc nil)))

(defn connect-player [game player]
  (let [loc (position-of-player-id game (:id player))
        position (or loc 0)
        status (g/player-status @game position)]
    (seat-player game position player)
    (summarize player status)))

(defn -main [& args]
  (cli-game))
