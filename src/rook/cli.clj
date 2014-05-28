(ns rook.cli
  (:require [clansi.core :refer [style]]
            [clojure.string :refer [trim-newline]]
            [clojure.pprint :refer [pprint]]
            [rook.protocols :refer [IPlayer]]
            [rook.game :as game]))

(def colors
  {:black :white
   :red :red
   :yellow :yellow
   :green :green})

(def labels
  {:black "B"
   :red "R"
   :yellow "Y"
   :green "G"})

(def suits
  (reduce-kv (fn [m k v] (assoc m v k)) {} labels))

(def ranks
  (apply hash-map
    (->> game/ranks
         (mapcat (fn [r] [(str r) r]))
         (cons :rook)
         (cons "R"))))

(defn format-card [card]
  (let [rank (:rank card)
        rank (if (= rank :rook) "R" rank)]
    (str rank (labels (:suit card)))))

(defn display-card [card]
  (style (format-card card) (colors (:suit card))))

(defn display-cards [cards]
  (let [sorter #(vector (:suit %) (:value %))]
    (->> cards
         (sort-by sorter)
         (mapv display-card))))

(defn print-status [{:keys [trick hand legal-moves]}]
  (println)
  (print "Played so far: ")
  (println (mapv display-card trick))
  (print "Your hand:     ")
  (println (display-cards hand))
  (print "Legal moves:   ")
  (println (display-cards legal-moves)))

(defn print-trick-summary [trick]
  (print "Trick: ")
  (println (mapv display-card trick)))

(defn print-score [score]
  (pprint score))

(defn parse-input [in]
  (when-let [matches (re-find #"^(\d{1,2}|R)([BGRY])" in)]
    (let [[_ rank suit] matches]
      {:suit (suits suit)
       :rank (ranks rank)})))

(defn get-input [valid]
  (println)
  (loop []
    (print "Your move: ") (flush)
    (let [input (trim-newline (read-line))
          valid (conj valid "quit")
          valid? (some #{input} valid)]
      (if valid?
        input
        (do
          (println "Illegal move. Valid: " valid)
          (recur))))))

(defn cli-player []
  (reify
    IPlayer
    (get-card [_ status]
      (let [{:keys [legal-moves]} status
            valid (map format-card legal-moves)
            input (get-input valid)]
        (parse-input input)))))
