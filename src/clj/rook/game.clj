(ns rook.game
  (:require [clojure.set :as set]))

(declare owner-of)

(def kitty-size 5)
(def player-count 4)

;; always 2 teams - odds vs evens
(def teams (map (fn [mems i] {:position i, :members (set mems)})
                ((juxt filter remove) even? (range player-count))
                (range)))

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

(defn- deal-hand [cards seat]
  (let [hand (map #(assoc % :seat seat) cards)]
    {:position seat
     :played-cards #{}
     :dealt-hand (set hand)}))

(defn- deal [cards player-count]
  (let [deck-size (/ (count cards) player-count)
        hands (partition deck-size cards)]
    (vec (map deal-hand hands (range)))))

(defn new-game []
  (let [cards (-> (deck-with-points house-points) shuffle)
        [kitty dealable-cards] (split-at kitty-size cards)
        seats (deal dealable-cards player-count)]
    {:seats seats
     :tricks []
     :bids []
     :players []
     :winning-bid nil
     :kitty (vec kitty)
     :trump nil}))

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

(defn add-bid [game seat bid]
  (update-in game [:bids] conj {:seat seat :bid bid}))

(defn wrapping-inc [n]
  (-> n inc (mod player-count)))

(defn next-bid-position [bidders prev]
  (if (< (count bidders) 2)
    prev
    (loop [n prev]
      (let [n (wrapping-inc n)]
        (if (bidders n)
          n
          (recur n))))))

(defn bid-status [game]
  (let [bids (:bids game)
        seats (set (range player-count))]
    (if (seq bids)
      (let [active-bids (remove (comp nil? :bid) bids)
            passed-bids (filter (comp nil? :bid) bids)
            bidders (set/difference seats (map :seat passed-bids))
            highest (apply max-key :bid active-bids)
            last-bid (peek bids)
            position (next-bid-position bidders (:seat last-bid))]
        {:bids bids
         :current-bid (:bid highest)
         :active-bidders bidders
         :highest highest
         :position position})
      {:bids bids
       :current-bid 0
       :active-bidders seats
       :highest nil
       :position (rand-int player-count)})))

(defn award-bid-winner [game]
  (assoc game :winning-bid (-> game bid-status :highest)))

(defn set-trump [game suit]
  (if-let [position (:position (owner-of (:seats game) rook))]
    (-> game
        (assoc :trump suit)
        (update-in [:seats position :dealt-hand] suit-up-rook suit)
        (update-in [:kitty] suit-up-rook suit))
    game))


(defn team-for-seat [seat]
  (some #(when (some #{seat} (:members %)) %) teams))

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
  (:seat (best-card trick trump)))

(defn team-winner-for-trick [trump trick]
  (team-for-seat (winner-for-trick trump trick)))

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

(defn next-seat-for-current-trick [game]
  (if-let [t (trick-in-play (:tricks game))]
    (-> t peek :seat wrapping-inc)))

(defn winner-of-previous-trick [game]
  (if-let [trick (peek (game :tricks))]
    (winner-for-trick (:trump game) trick)))

;; Beginning of game -> random first seat (eventually left of "dealer")
(defn next-seat [game]
  (cond
    (beginning-of-game? game) (wrapping-inc (or (some-> game :winning-bid :seat)
                                                (rand-int player-count)))
    (trick-in-play (:tricks game)) (next-seat-for-current-trick game)
    :else (winner-of-previous-trick game)))

(defn owner-of
  "Returns the seat that owns this card"
  [seats card]
  (some (fn [seat]
          (when (card-in-hand? (:dealt-hand seat) card)
            seat))
        seats ))

(defn- add-card-to-tricks [tricks card]
  (if-let [trick (trick-in-play tricks)]
    (conj (vec (butlast tricks)) (conj trick card))
    (conj tricks [card])))

(defn unplayed-cards [game position]
  (let [seat (get-in game [:seats position])]
    (set/difference (:dealt-hand seat)
                    (:played-cards seat))))

(defn play [game card]
  (let [seat (owner-of (:seats game) card)
        position (:position seat)
        hand (get-in game [:seats position :dealt-hand])
        card (find-card hand card)]
    (-> game
        (update-in [:seats position :played-cards] conj card)
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

(defn adjust-for-bid [bid scores]
  (let [{:keys [seat bid]} bid]
    (reduce (fn [all {:keys [members score] :as team-score}]
              (conj all (if (and (members seat) (< score bid))
                          (assoc team-score :score (- bid))
                          team-score)))
            [] scores)))

(defn score-before-bid-adjustment [game]
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

(defn score [game]
  (let [adjust (partial adjust-for-bid (:winning-bid game))]
    (-> game
        score-before-bid-adjustment
        adjust)))

(defn status [game]
  (let [tricks (:tricks game)
        trick (trick-in-play tricks)
        position (next-seat game)
        cards (unplayed-cards game position)
        led (first trick)
        legal (legal-moves cards (:suit led))]
    {:position position
     :hand cards
     :led led
     :legal-moves legal
     :trump (:trump game)
     :trick trick
     :previous-trick (when-not trick (peek tricks))
     :trick-number (count tricks)}))

(defn trick-summary [game trick]
  (let [winning-card (best-card trick (:trump game))
        position (:seat winning-card)
        team (team-for-seat position)]
    {:trick trick
     :winning-card winning-card
     :winning-position position
     :winning-team team}))
