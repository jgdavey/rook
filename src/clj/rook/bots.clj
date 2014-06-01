(ns rook.bots
  (:require [rook.game :as game]
            [rook.protocols :as p :refer [IPlayer]]))

(defn raise-bid [bid-status n up-to]
  (let [bids (count (:bids bid-status))
        current-bid (:current-bid bid-status)]
    (if (zero? bids)
      75
      (when (< current-bid up-to)
        (+ current-bid n)))))

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
  [name]
  (reify
    p/IPlayer
    (get-card [_ status]
      (if (partner-has-it? status)
        (or (point-card status)
            (worst-card status))
        (or (better-card status)
            (worst-card status))))
    (display-name [_] (str name " (intermediate)"))
    (get-bid [_ status]
      (raise-bid status 15 145))))

(defn simple-bot
  "Plays any better card than what's out there, otherwise worst card"
  [name]
  (reify
    p/IPlayer
    (get-card [_ status]
        (or (better-card status)
            (worst-card status)))
    (display-name [_] (str name " (simple)"))
    (get-bid [_ status]
      (raise-bid status 10 140))))

(defn stupid-bot
  "Always plays the 'first' legal move, without regard for what's been played"
  [name]
  (reify
    p/IPlayer
    (get-card [_ status]
      (let [{:keys [legal-moves]} status]
        (first legal-moves)))
    (display-name [_] (str name " (stupid)"))
    (get-bid [_ status]
      (raise-bid status 20 150))))
