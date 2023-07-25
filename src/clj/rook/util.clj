(ns rook.util
  (:require
   [clojure.string :as str]
   [clansi.core :as ansi]))

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

(defn format-card [card]
  (let [rank (:rank card)
        rank (if (= rank :rook) "R" rank)]
    (str rank (labels (:suit card)))))

(defn colorize [string suit]
  (ansi/style string :bright (colors suit)))

(defn display-card [card]
  (colorize (format-card card) (:suit card)))

(defn display-cards [cards]
  (let [sorter #(vector (:suit %) (:value %))]
    (->> cards
         (sort-by sorter)
         (mapv display-card))))

(defn display-cards-with-legal [cards legal]
  (let [sorter #(vector (:suit %) (:value %))
        legal (set legal)]
    (->> cards
         (sort-by sorter)
         (mapv (fn [card]
                 (cond-> (display-card card)
                   (contains? legal card) (ansi/style :underline)))))))

(defn display-cards-string [cards]
  (str "["
       (str/join " " (display-cards cards))
       "]"))
