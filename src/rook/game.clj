(ns rook.game
  (:require [clojure.set :as set]))

(declare owner-of)

(def kitty-size 5)
(def player-count 4)

;; always 2 teams - odds vs evens
(def teams (map #(hash-map :members (set %))
                ((juxt filter remove) even? (range player-count))))

(def ranks (vec (range 1 15)))
(def suits [:red :green :black :yellow])

(def rook {:rank :rook, :suit :rook, :value 0})

(def base-deck
  (cons rook
        (for [i ranks
              s suits]
          {:rank i
           :suit s
           :value (if (= i 1) 15 i)})))

(def house-points
  {5  5
   10 10
   14 10
   1  15
   :rook 20})

;; Game setup

(defn- deck-with-points [point-map]
  (map (fn [card]
         (assoc card :points (get point-map (:rank card) 0))) base-deck))

(defn- deal-hand [cards player]
  (let [hand (map #(assoc % :player player) cards)]
    {:label (str "player" (inc player))
     :position player
     :played-cards #{}
     :dealt-hand (set hand)}))

(defn- deal [cards player-count]
  (let [deck-size (/ (count cards) player-count)
        hands (partition deck-size cards)]
    (vec (map deal-hand hands (range)))))

(defn new-game []
  (let [cards (-> (deck-with-points house-points) shuffle)
        [kitty dealable-cards] (split-at kitty-size cards)
        players (deal dealable-cards player-count)]
    {:players players
     :tricks []
     :kitty (vec kitty)
     :trump :black}))

(defn cards-equal? [card1 card2]
  (and (= (:suit card1) (:suit card2))
       (= (:rank card1) (:rank card2))))

(defn find-card [hand rank-and-suit]
  (some (fn [card] (when (cards-equal? card rank-and-suit) card)) hand))

(defn card-in-hand? [hand card]
  (boolean (find-card hand card)))

(defn- suit-up-rook [hand suit]
  (if-let [old-rook (some (fn [c] (when (cards-equal? rook c) c)) hand)]
    (let [new-rook (assoc old-rook :suit suit)]
      (-> hand
          (disj old-rook)
          (conj new-rook)))
    hand))

(defn set-trump [game suit]
  (if-let [position (:position (owner-of (:players game) rook))]
    (-> game
        (assoc :trump suit)
        (update-in [:players position :dealt-hand] suit-up-rook suit)
        (update-in [:kitty] suit-up-rook suit))
    game))


(defn team-for-player [player]
  (some #(when (some #{player} (:members %)) %) teams))

(defn suited
  "Returns a fn that returns true for a map with :suit suit"
  [suit]
  (comp (partial = suit) :suit))

(defn best-of-suit [trick suit]
  (let [of-suit (filter (suited suit) trick)]
    (and (seq of-suit)
         (apply max-key :value of-suit))))

(defn best-card [trick trump]
  (let [suit (:suit (first trick))
        best (partial best-of-suit trick)]
    (or (best trump) (best suit))))

(defn winner-for-trick [trump trick]
  (:player (best-card trick trump)))

(defn team-winner-for-trick [trump trick]
  (team-for-player (winner-for-trick trump trick)))

(defn beginning-of-game?
  "True when no cards have been played yet"
  [game]
  (not (peek (game :tricks))))

(defn trick-in-play
  "A trick is in play if someone has lead, but all cards have not yet been played"
  [tricks]
  (when-let [trick (peek tricks)]
    (let [cards-played (count trick)]
      (when (and (> cards-played 0) (< cards-played player-count))
        trick))))

(defn next-player-for-current-trick [game]
  (if-let [t (trick-in-play (:tricks game))]
    (-> t peek :player inc (mod player-count))))

(defn winner-of-previous-trick [game]
  (if-let [trick (peek (game :tricks))]
    (winner-for-trick (:trump game) trick)))

;; Beginning of game -> random first player (eventually left of "dealer")
(defn next-player [game]
  (cond
    (beginning-of-game? game) (rand-int player-count)
    (trick-in-play (:tricks game)) (next-player-for-current-trick game)
    :else (winner-of-previous-trick game)))

(defn owner-of
  "Returns the player that owns this card"
  [players card]
  (some (fn [player]
          (when (card-in-hand? (:dealt-hand player) card)
            player))
        players ))

(defn- add-card-to-tricks [tricks card]
  (if-let [trick (trick-in-play tricks)]
    (conj (vec (butlast tricks)) (conj trick card))
    (conj tricks [card])))

(defn unplayed-cards [game position]
  (let [player (get-in game [:players position])]
    (set/difference (:dealt-hand player)
                    (:played-cards player))))

(defn play [game card]
  (let [player (owner-of (:players game) card)
        position (:position player)
        hand (get-in game [:players position :dealt-hand])
        card (find-card hand card)]
    (-> game
        (update-in [:players position :played-cards] conj card)
        (update-in [:tricks] add-card-to-tricks card))))

(defn legal-moves [hand suit-led]
  (let [matches (filterv (suited suit-led) hand)]
    (if (and suit-led (seq matches))
      matches
      hand)))

;; Scoring

(defn- sum-points [trick]
  (reduce (fn [total trick] (+ total (:points trick))) 0 trick))

(defn- kitty-score [winner kitty]
  (let [points (sum-points kitty)]
    {winner points}))

(defn- base-score [teams-and-tricks]
  (reduce-kv (fn [all team tricks]
               (assoc all team
                      (apply + (map sum-points tricks))))
             {} teams-and-tricks))

(defn most-tricks-score [tricks]
  (if-let [team (ffirst (sort-by (comp - count val) tricks))]
    {team 20}
    {}))

(defn score [game]
  (let [team (partial team-winner-for-trick (:trump game))
        tricks (group-by team (:tricks game))
        base (base-score tricks)
        kitty (kitty-score (team (peek (:tricks game)))
                           (:kitty game))
        most-tricks (most-tricks-score tricks)]
    (reduce-kv (fn [all team score]
                 (conj all
                       (assoc team :score score)))
               []
               (merge-with + base kitty most-tricks))))

(defn status [game]
  (let [tricks (:tricks game)
        trick (trick-in-play tricks)
        position (next-player game)
        cards (unplayed-cards game position)
        led (first trick)
        legal (legal-moves cards (:suit led))]
    {:position position
     :hand cards
     :led led
     :legal-moves legal
     :trump (:trump game)
     :trick trick
     :trick-number (count tricks)}))
