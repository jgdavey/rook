(ns rook.cli
  (:require [clansi.core :refer [style]]
            [clojure.string :as str :refer [trim-newline]]
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

(defn print-hand-summary [hand]
  (println "Your hand:" (display-cards hand)))

(defn print-initial-game-summary [position game-summary]
  (let [{:keys [seats]} game-summary
        hand (-> seats (get position) :dealt-hand)]
    (println)
    (println "*** Welcome to Rook ***")
    (println)
    (print-hand-summary hand)))

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

(defn parse-bid [in]
  (when-let [match (re-find #"^\d{2,3}$" in)]
    (Integer. match)))

(defn get-inputs [validator print-chan]
  (loop []
    (let [raw (trim-newline (read-line))
          inputs (str/split raw #" ")
          valid? (:fn validator)]
      (if (valid? inputs)
        inputs
        (do
          (put! print-chan [:print "\nIllegal move. Valid:" (:set validator) "\n> "])
          (recur))))))

(defn validator
  ([in-set] (validator in-set (constantly true)))
  ([in-set additional]
   (let [s (set in-set)]
     {:fn
      (fn [coll]
        (and (every? s coll)
             (additional coll)))
      :set s})))

(defn print-bid-won [player bid]
  (println (display-name player) "took the bid at" bid)
  (println))

(defn print-bid [player bid]
  (let [message (if bid (str "bid " bid) "passed")]
    (println (display-name player) message)))

(defn print-trump [trump]
  (println "Trump is" (style (name trump) (colors trump))))

(defn print-message [& messages]
  (binding [*print-readably* nil]
    (apply print (map pr-str messages)))
  (flush))

(def dispatch
  {:trick-summary print-trick-summary
   :card-played print-card-played
   :score print-score
   :print print-message
   :trump-chosen print-trump
   :bid print-bid
   :bid-won print-bid-won})

(defn subscribe [channel position]
  (let [sub-chan (chan)
        interests (assoc dispatch
                         [:player-status position] print-status
                         [:hand-summary position] print-hand-summary
                         :summary (partial print-initial-game-summary position))]
    (doseq [topic (keys interests)]
      (sub channel topic sub-chan))
    (go (loop []
          (when-let [[key & rest] (<! sub-chan)]
            (apply (interests key) rest)
            (recur))))
    sub-chan))

(defn cli-player [pub-chan position]
  (let [print-chan (subscribe pub-chan position)
        printer (fn [& msgs] (put! print-chan [:print (apply str msgs)]))]
    (reify
      IPlayer
      (get-card [_ status]
        (let [{:keys [legal-moves]} status
              valid (cons "quit" (map format-card legal-moves))
              validate (validator valid)
              [input] (get-inputs validate print-chan)]
          (parse-input input)))

      (get-bid [_ status]
        (let [{:keys [current-bid]} status
              valid (cons "pass" (map str (range (+ 5 current-bid) 205 5)))
              validate (validator valid)
              [input] (get-inputs validate print-chan)]
          (parse-bid input)))

      (display-name [_] "You")

      (choose-trump [_ hand]
        (printer "Choose trump\n> ")
        (let [validate (validator (keys suits))
              [input] (get-inputs validate print-chan)
              trump (suits input)]
          trump))

      (choose-new-kitty [_ hand-and-kitty]
        (printer "Choose new kitty\n> ")
        (let [valid (cons "" (map format-card hand-and-kitty))
              validate (validator valid (fn [in] (or ( = '("") in)
                                                   (= (count in) 5))))
              inputs (get-inputs validate print-chan)
              kitty (map parse-input inputs)]
          (set kitty))))))
