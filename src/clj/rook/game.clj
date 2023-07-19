(ns rook.game
  (:require [clojure.set :as set]))

(declare owner-of)

(defmacro dbg  [& body]
  `(let  [x# ~@body]
     (println (str "dbg: " (quote ~@body) "=" x#))
     x#))

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
  {:position seat
   :played-cards #{}
   :dealt-hand (set cards)})

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
     :kitty (set kitty)
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
  (let [bid (when (number? bid) bid)]
    (update-in game [:bids] conj {:seat seat :bid bid})))

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

(def ^:private nil-bid? (comp nil? :bid))

(defn bid-status [game]
  (let [bids (:bids game)
        seats (set (range player-count))
        active-bids (remove nil-bid? bids)
        passed-bids (filter nil-bid? bids)
        bidders (set/difference seats (into #{} (map :seat) passed-bids))
        highest (when (seq active-bids) (apply max-key :bid active-bids))
        last-bid (when (seq bids) (peek bids))
        position (if last-bid
                   (next-bid-position bidders (:seat last-bid))
                   (rand-int player-count))
        hand (get-in game [:seats position :dealt-hand])]
    {:bids bids
     :current-bid (or (:bid highest) 70)
     :active-bidders bidders
     :highest highest
     :position position
     :hand hand}))

(defn award-bid-winner [game]
  (assoc game :winning-bid (-> game bid-status :highest)))


(defn set-trump [game suit]
  (if-let [position (:position (owner-of (:seats game) rook))]
    (-> game
        (assoc :trump suit)
        (update-in [:seats position :dealt-hand] suit-up-rook suit)
        (update-in [:kitty] suit-up-rook suit))
    (assoc game :trump suit)))

;; Really just here for simulation
(defn ensure-trump [game]
  (if (:trump game)
    game
    (let [seat (rand-int player-count)
          best-suit (->> (get-in game [:seats seat :dealt-hand])
                         (map :suit)
                         frequencies
                         (apply max-key val)
                         first)]
      (-> game
          (assoc :winning-bid {:seat seat :bid 125})
          (set-trump best-suit)))))

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

(defn choose-new-kitty [game position new-kitty]
  (let [old-hand (get-in game [:seats position :dealt-hand])
        old-kitty (:kitty game)
        all (set/union old-hand old-kitty)
        new-kitty (remove nil? (map (partial find-card all) new-kitty))
        new-hand (set/difference all (set new-kitty))]
    (if (= kitty-size (count new-kitty))
      (-> game
          (assoc :kitty new-kitty)
          (update-in [:seats position :dealt-hand] (constantly (set new-hand))))
      game)))

(defn play* [game seat card]
  (-> game
      (update-in [:seats seat :played-cards] conj card)
      (update-in [:tricks] add-card-to-tricks (assoc card :seat seat))))

(defn play [game card]
  (let [seat (owner-of (:seats game) card)
        position (:position seat)
        hand (get-in game [:seats position :dealt-hand])
        card (find-card hand card)]
    (play* game position card)))

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
             (reduce #(assoc %1 %2 0) {} teams)
             teams-and-tricks))

(defn most-tricks-score [tricks]
  (if-let [team (ffirst (sort-by (comp - count val) tricks))]
    {team 20}
    {}))

(defn adjust-for-bid [bid scores]
  (let [{:keys [seat bid]} bid]
    (reduce (fn [all {:keys [members score] :as team-score}]
              (conj all (if (and members score (members seat) (< score bid))
                          (assoc team-score :score (- bid))
                          team-score)))
            [] scores)))

(defn score-before-bid-adjustment [game]
  (let [team (partial team-winner-for-trick (:trump game))
        tricks (group-by team (:tricks game))
        base (base-score tricks)
        full-deck (into (set (:kitty game)) (mapcat :dealt-hand (:seats game)))
        played-cards (into #{} (mapcat :played-cards (:seats game)))
        kitty-as-played (set/difference full-deck played-cards)
        kitty (kitty-score (team (peek (:tricks game)))
                           kitty-as-played)
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

(defn game-over? [game]
  (let [tricks (:tricks game)
        total-tricks (/ (- (count base-deck) kitty-size) player-count)]
    (and tricks
         (= total-tricks (count tricks))
         (= player-count (count (peek tricks))))))

(defn status [game]
  (let [tricks (:tricks game)
        trick (trick-in-play tricks)
        position (next-seat game)
        cards (unplayed-cards game position)
        led (first trick)
        legal (legal-moves cards (:suit led))]
    {:game game
     :position position
     :hand cards
     :led led
     :legal-moves legal
     :trump (:trump game)
     :trick trick
     :previous-trick (when-not trick (peek tricks))
     :tricks (cond-> tricks trick pop)
     :played-cards (into #{}
                         (mapcat :played-cards)
                         (:seats game))
     :trick-number (count tricks)}))

(defn player-summary [game seat]
  (let [tricks (:tricks game)
        trick (trick-in-play tricks)
        cards (unplayed-cards game seat)
        led (first trick)
        legal (legal-moves cards (:suit led))]
    {:position seat
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
