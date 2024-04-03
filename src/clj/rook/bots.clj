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
    (-> (+ (* 1.75 best-value)
           (* (- best-count 4) 20)
           (* 20 ones)
           (* 7.5 fourteens))
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
  (let [candidate-cards (remove (fn [{:keys [value rank]}]
                                    (or
                                     (= :rook rank)
                                     (> value 13)))
                                  cards)
        stats (suit-stats candidate-cards)
        least (:cards (apply min-key :count stats))
        lowest (sort-by :value candidate-cards)]
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

(defn- lead-key-fn [card]
  (cond
    (= (:rank card) 1) -20
    (zero? (:points card)) (- (:value card))
    :else (:value card)))

(defn mcts-bot
  "Choose cards based on monte-carlo tree search "
  [mcts-config]
  (reify
    IBot
    (get-card [_ {:keys [game legal-moves]}]
      (if (game/beginning-of-game? game)
        ;; best off-suit play
        (first
         (sort-by
          lead-key-fn
          (remove #(= (:suit %) (:trump game)) legal-moves)))
        (mcts/choose-next-card game mcts-config)))
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

(def strategies {:simple (simple-bot)
                 :stupid (stupid-bot)
                 :intermediate (intermediate-bot)
                 :mcts (mcts-bot {:iterations 2500
                                  :max-tree-depth 15
                                  :max-tree-width 15})})

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

(defn resolve-strategy [strategy]
  (cond
    (contains? strategies strategy) (strategies strategy)
    (satisfies? IBot strategy)      strategy
    :else                           (throw (ex-info "Strategy is not a valid IBot strategy" {}))))

(defn bot [& {:keys [in out name strategy] :or {strategy :simple
                                                name "bot"
                                                in (chan 1)
                                                out (chan 1)}}]
  (let [impl (resolve-strategy strategy)
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
  (loop [g (assoc game :action-ts [] :action-base-ts (. System (nanoTime)))]
    (if (game/game-over? g)
      g
      (let [seat (game/next-seat g)
            bot (nth bots seat)
            card (get-card bot (game/status g))
            new-game (game/play g seat card)]
        (recur (update new-game :action-ts conj (. System (nanoTime))))))))

(defn simulate [game strats]
  (let [bots (->> (or (not-empty strats) [:intermediate])
                  cycle
                  (map resolve-strategy)
                  (take game/player-count))]
    (-> game
        (simulate-bids bots)
        (simulate-trump-and-kitty bots)
        (simulate-play bots))))

(defn run-a-simulation [strats]
  (let [start (. System (nanoTime))
        game (game/new-game)
        played (simulate game strats)
        score (game/score played)
        won? (every? pos? (map :score score))]
    {:winning-bid (get played :winning-bid)
     :won? won?
     :score score
     :game (dissoc played :action-ts :action-base-ts)
     :action-times (->> (seq (:action-ts played))
                        (cons (:action-base-ts played))
                        (partition 2 1)
                        (map #(/ (double (apply - (reverse %))) 1000000.0))
                        vec)
     :time-ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)}))

(comment

  (time
   (let [n (async/to-chan!! (range 50))
         out (async/chan 1)
         _ (async/pipeline 4 out (map (fn [_] (run-a-simulation [:intermediate
                                                                 (mcts-bot {:iterations 500
                                                                            :max-tree-width 12})]))) n)]
     (def results (async/<!! (async/into [] out)))
     (println "Done")))

  (run-a-simulation [:intermediate])

  (run-a-simulation [(mcts-bot {:iterations 1000})])

  (frequencies (map #(if (= 1 (mod (get-in % [:winning-bid :seat]) 2))
                       (:won? %)
                       (not (:won? %))) results))

  :okay)
