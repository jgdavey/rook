(ns rook.cli
  (:require [clansi.core :refer [style]]
            [clojure.string :refer [trim-newline]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as async :refer [<! <!! chan go sub put!]]
            [rook.protocols :refer :all]
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
  (print "\nPlayed so far: "
         (mapv display-card trick)
         "\nYour hand:     "
         (display-cards hand)
         "\nLegal moves:   "
         (display-cards legal-moves)
         "\n> ")
  (flush))

(defn print-initial-game-summary [game-summary]
  (let [{:keys [trump]} game-summary]
    (println)
    (println "Welcome to Rook")
    (print "Trump: ")
    (println (style trump (colors trump)))))

(defn print-score [score]
  (pprint score))

(defn print-trick-summary [trick-summary]
  (let [{:keys [trick winning-card winning-team]} trick-summary
        team-name (str "[team " (:position winning-team) "]")]
    (print "\nTrick: ")
    (println (mapv display-card trick))
    (println (display-card winning-card) team-name "wins trick")
    (println)))

(defn print-card-played [player card]
  (println (display-name player) "played" (display-card card)))

(defn parse-input [in]
  (when-let [matches (re-find #"^(\d{1,2}|R)([BGRY])" in)]
    (let [[_ rank suit] matches]
      {:suit (suits suit)
       :rank (ranks rank)})))

(defn get-input [valid print-chan]
  (loop []
    (let [input (trim-newline (read-line))
          valid (conj valid "quit")
          valid? (some #{input} valid)]
      (if valid?
        input
        (do
          (put! print-chan [:print "\nIllegal move. Valid:" valid "\n> "])
          (recur))))))

(defn print-message [& messages]
  (binding [*print-readably* nil]
    (apply print (map pr-str messages)))
  (flush))

(def dispatch
  {:summary print-initial-game-summary
   :trick-summary print-trick-summary
   :card-played print-card-played
   :score print-score
   :print print-message})

(defn subscribe [channel position]
  (let [sub-chan (chan)
        interests (assoc dispatch [:player-status position] print-status)]
    (doseq [topic (keys interests)]
      (sub channel topic sub-chan))
    (go (loop []
          (when-let [[key & rest] (<! sub-chan)]
            (apply (interests key) rest)
            (recur))))
    sub-chan))

(defn cli-player [pub-chan position]
  (let [print-chan (subscribe pub-chan position)]
    (reify
      IPlayer
      (get-card [_ status]
        (let [{:keys [legal-moves]} status
              valid (map format-card legal-moves)
              input (get-input valid print-chan)]
          (parse-input input)))
      (display-name [_] "You"))))
