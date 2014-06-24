(ns rook.bots
  (:require [rook.game :as game]
            [clojure.core.async :as async :refer [chan]]
            [rook.seat :as seat]))

(defprotocol IBot
  (get-card [this status])
  (get-bid [this bid-status])
  (choose-new-kitty [this hand-and-kitty])
  (choose-trump [this hand]))

(defn raise-bid [bid-status n up-to]
  (let [bids (count (:bids bid-status))
        current-bid (:current-bid bid-status)]
    (if (zero? bids)
      75
      (if (< current-bid up-to)
        (+ current-bid n)
        :pass))))

(defn- trump-fn [trump-suit]
  (comp (partial = trump-suit) :suit))

(defn- sortable [trump]
  (let [trump? (trump-fn trump)]
    (fn [card]
      (vec ((juxt trump? :value) card)))))

(defn- partner-has-it? [{:keys [trump trick]}]
  (let [best-in-trick (last (sort-by (sortable trump) trick))]
    (and (> 1 (count trick))
         (= (peek (pop trick)) best-in-trick))))

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

(defn expendible [cards]
  (let [stats (suit-stats cards)
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
      (raise-bid status 15 145))))

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
      (raise-bid status 10 140))))

(defn stupid-bot
  "Always plays the 'first' legal move, without regard for what's been played"
  []
  (reify
    IBot
    (p/get-card [_ status]
      (let [{:keys [legal-moves]} status]
        (first legal-moves)))
    (p/choose-new-kitty [_ hand-and-kitty]
      #{})
    (p/choose-trump [_ hand]
      :red)
    (p/get-bid [_ status]
      (raise-bid status 20 150))))

(def strategies {:simple simple-bot
                 :stupid stupid-bot
                 :intermediate intermediate-bot})

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
