(ns rook.bots
  (:require
   [clojure.core.async :as async :refer [chan]]
   [clojure.set :as set]
   [rook.game :as game]
   [rook.mcts :as mcts]
   [rook.util]
   [rook.seat :as seat]))

(defprotocol IBot
  (get-card [this status])
  (get-bid [this bid-status])
  (choose-new-kitty [this hand-and-kitty])
  (choose-trump [this hand]))

(defn- rook? [card]
  (= :rook (:rank card)))

(defn suit-stats [cards]
  (let [grouped (->> cards
                     (remove rook?)
                     (group-by :suit))]
    (reduce-kv (fn [all suit cards]
                 (conj all {:suit suit
                            :cards cards
                            :count (count cards)
                            :value (reduce + (map :value cards))}))
               [] grouped)))

(defn maximum-potential-bid [cards]
  (let [stats (suit-stats cards)
        ones (->> cards (filter #(= (:value %) 15)) count)
        fourteens (->> cards (filter #(= (:value %) 14)) count)
        {best-count :count best-value :value} (apply max-key :value stats)]
    (-> (+ (* 1.6 best-value)
           (* (- best-count 4) 20)
           (* 18 ones)
           (* 9 fourteens))
        (/ 5.0)
        (Math/round)
        (* 5)
        (max 75)
        (min 200))))

(defn raise-bid [{:keys [hand bids current-bid]} n]
  (let [num-bids (count bids)
        maximum (maximum-potential-bid hand)
        next-bid (+ current-bid n (rand-nth [0 0 0 0 5 5 10]))]
    (cond
      (zero? num-bids) next-bid
      (< next-bid maximum) next-bid
      :else :pass)))

(defn- trump-fn [trump-suit]
  (comp (partial = trump-suit) :suit))

(defn- sortable [trump]
  (let [trump? (trump-fn trump)]
    (juxt trump? :value)))

(defn- partner-has-it? [{:keys [trump trick]}]
  (let [best-in-trick (last (sort-by (sortable trump) trick))]
    (and (> 1 (count trick))
         (= (peek (pop trick)) best-in-trick))))

(defn expendible [cards]
  (let [stats (suit-stats (filter #(< (:value %) 14) cards))
        least (:cards (apply min-key :count stats))
        lowest (sort-by :value cards)]
    (concat least (remove (set least) lowest))))

(defn better-card [{:keys [trump legal-moves trick]}]
  (let [s (sortable trump)]
    (when-let [best-in-trick (last (sort-by s trick))]
      (some (fn [card]
              (when (< (compare (s best-in-trick) (s card)) 0)
                card)) legal-moves))))

(defn point-card
  "Returns lowest-ranked point card, excluding trump cards"
  [{:keys [trump legal-moves]}]
  (let [possible (remove (trump-fn trump) legal-moves)
        pointed (remove (comp zero? :points) possible)]
    (first (sort-by :value pointed))))

(defn worst-card [{:keys [trump legal-moves trick]}]
  (first (sort-by (sortable trump) legal-moves)))

(defn mcts-bot
  "Choose cards based on monte-carlo tree search "
  []
  (reify
    IBot
    (get-card [_ {:keys [game]}]
      (mcts/choose-next-card game 800))
    (choose-trump [_ hand]
      (->> hand
           suit-stats
           (apply max-key :count)
           :suit))
    (choose-new-kitty [_ cards]
      (set (take 5 (expendible cards))))
    (get-bid [_ status]
      (raise-bid status 10))))

(defn intermediate-bot
  "If partner has the trick, and bot is the last to play, tries to play points.
  Otherwise, just like simple bot"
  []
  (reify
    IBot
    (get-card [_ status]
      (if (partner-has-it? status)
        (or (point-card status)
            (worst-card status))
        (or (better-card status)
            (worst-card status))))
    (choose-trump [_ hand]
      (->> hand
           suit-stats
           (apply max-key :count)
           :suit))
    (choose-new-kitty [_ cards]
      (set (take 5 (expendible cards))))
    (get-bid [_ status]
      (raise-bid status 10))))

(defn simple-bot
  "Plays any better card than what's out there, otherwise worst card"
  []
  (reify
    IBot
    (get-card [_ status]
        (or (better-card status)
            (worst-card status)))
    (choose-trump [_ hand]
      (->> hand
           suit-stats
           (apply max-key :count)
           :suit))
    (choose-new-kitty [_ cards]
      (set (take 5 (expendible cards))))
    (get-bid [_ status]
      (raise-bid status 10))))

(defn stupid-bot
  "Always plays the 'first' legal move, without regard for what's been played"
  []
  (reify
    IBot
    (get-card [_ status]
      (let [{:keys [legal-moves]} status]
        (first legal-moves)))
    (choose-new-kitty [_ hand-and-kitty]
      #{})
    (choose-trump [_ hand]
      :red)
    (get-bid [_ status]
      (raise-bid status 20))))

(def strategies {:simple simple-bot
                 :stupid stupid-bot
                 :intermediate intermediate-bot
                 :mcts mcts-bot})

(def responses {:rook/get-bid get-bid
                :rook/get-card get-card
                :rook/choose-kitty choose-new-kitty
                :rook/choose-trump choose-trump })

(defrecord BotSeat [in out name strategy]
  seat/IGoable
  (seat/in [_] in)
  (seat/out [_] out)
  (seat/dispatch [_ [topic body]]
    (when-let [f (responses topic)]
      (f strategy body)))

  seat/ISeat
  (seat/display-name [_] name)

  seat/IConnected
  (seat/connected? [_] true))

(defn bot [& {:keys [in out name strategy] :or {strategy :simple
                                                name "bot"
                                                in (chan 1)
                                                out (chan 1)}}]
  (let [name (str name " (" strategy ")")
        impl ((strategies strategy))
        seat (->BotSeat in out name impl)]
    (seat/go-seat seat)
    seat))

(defn simulate-bids [game bots]
  (loop [game game]
    (let [bid-status (game/bid-status game)]
      (if (> (count (:active-bidders bid-status)) 1)
        (let [position (:position bid-status)
              bid (get-bid (nth bots position) bid-status)]
          (recur (game/add-bid game position bid)))
        (game/award-bid-winner game)))))

(defn simulate-trump-and-kitty [game bots]
  (let [seat (get-in game [:winning-bid :seat])
        bot (nth bots seat)
        hand (get-in game [:seats seat :dealt-hand])
        hand+kitty (into hand (get-in game [:kitty]))
        new-kitty (choose-new-kitty bot hand+kitty)
        new-hand (set/difference hand+kitty new-kitty)]
    (-> game
        (game/choose-new-kitty seat new-kitty)
        (game/set-trump (choose-trump bot new-hand)))))

(defn simulate-play [game bots]
  (loop [g game]
    (if (game/game-over? g)
      g
      (let [seat (game/next-seat g)
            bot (nth bots seat)
            card (get-card bot (game/status g))]
        (recur
         (game/play* g seat card))))))

(defn simulate [game strats]
  (let [bots (->> (or (not-empty strats) [:intermediate])
                  cycle
                  (map #((strategies %)))
                  (take game/player-count))]
    (-> game
        (simulate-bids bots)
        (simulate-trump-and-kitty bots)
        (simulate-play bots))))

(comment

  (let [start (. System (nanoTime))
        game (game/new-game)
        played (simulate game [:mcts])
        score (game/score played)]
    {:winning-bid (get played :winning-bid)
     :score score
     :time-ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)})

  :okay)
