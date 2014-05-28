(ns rook.bots
  (:require [rook.game :as game]
            [rook.protocols :as p :refer [IPlayer]]))

(defn- sortable [trump]
  (let [trump? (comp (partial = trump) :suit)]
    (fn [card]
      (vec ((juxt trump? :value) card)))))

(defn stupid-bot
  "Always plays the 'first' legal move, without regard for what's been played"
  []
  (reify
    p/IPlayer
    (get-card [_ status]
      (let [{:keys [legal-moves]} status]
        (first legal-moves)))))

(defn better-card [{:keys [trump legal-moves trick]}]
  (let [s (sortable trump)]
    (when-let [best-in-trick (last (sort-by s trick))]
      (some (fn [card]
              (when (< (compare (s best-in-trick) (s card)) 0)
                card)) legal-moves))))

(defn worst-card [{:keys [trump legal-moves trick]}]
  (first (sort-by (sortable trump) legal-moves)))

(defn simple-bot
  "Plays any better card than what's out there, otherwise worst card"
  []
  (reify
    p/IPlayer
    (get-card [_ status]
        (or (better-card status)
            (worst-card status)))))
