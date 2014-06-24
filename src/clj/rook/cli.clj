(ns rook.cli
  (:require [clansi.core :refer [style]]
            [clojure.string :as str :refer [trim-newline]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as async :refer [chan]]
            [rook.seat :as seat]
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
  (println "\nPlayed so far: "
         (mapv display-card trick)
         "\nYour hand:     "
         (display-cards hand)
         "\nLegal moves:   "
         (display-cards legal-moves)))

(defn print-hand-summary [hand]
  (println "Your hand:" (display-cards hand)))

(defn print-initial-game-summary [{:keys [hand]}]
  (println)
  (println "*** Welcome to Rook ***")
  (println)
  (print-hand-summary hand))

(defn print-score [score]
  (doseq [team (sort-by :position score)]
    (println "Team" (:members team) "scored" (:score team))))

(defn print-trick-summary [trick-summary]
  (let [{:keys [trick winning-card winning-team]} trick-summary
        team-name (str "[team " (:position winning-team) "]")]
    (print "\nTrick: ")
    (println (mapv display-card trick))
    (println (display-card winning-card) team-name "wins trick")
    (println)))

(defn print-card-played [[player card]]
  (println player "played" (display-card card)))

(defn parse-input [in]
  (if (= "quit" in)
    :quit
    (if-let [matches (re-find #"^(\d{1,2}|R)([BGRY])" in)]
      (let [[_ rank suit] matches]
        {:suit (suits suit)
         :rank (ranks rank)}))))

(defn parse-bid [in]
  (if-let [match (re-find #"^\d{2,3}$" in)]
    (Integer. match)
    :pass))

(defn get-inputs [validator]
  (loop []
    (print "> ") (flush)
    (let [raw (trim-newline (read-line))
          inputs (str/split raw #" ")
          valid? (:fn validator)]
      (if (valid? inputs)
        inputs
        (do
          (println "\nIllegal move. Valid:" (:set validator))
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

(defn print-bid-won [[player bid]]
  (println player "took the bid at" bid)
  (println))

(defn print-bid [[player bid]]
  (let [message (if bid (str "bid " bid) "passed")]
    (println player message)))

(defn print-trump [trump]
  (println "Trump is" (style (name trump) (colors trump))))

(defn print-message [& messages]
  (binding [*print-readably* nil]
    (apply print (map pr-str messages)))
  (flush))

(defn get-card [{:keys [legal-moves] :as status}]
  (print-status status)
  (let [valid (cons "quit" (map format-card legal-moves))
        validate (validator valid)
        [input] (get-inputs validate)]
    (parse-input input)))

(defn get-bid [status]
  (print "Your bid")
  (let [{:keys [current-bid]} status
        valid (cons "pass" (map str (range (+ 5 current-bid) 205 5)))
        validate (validator valid)
        [input] (get-inputs validate)]
    (parse-bid input)))

(defn choose-trump [hand]
  (print "Choose trump")
  (let [validate (validator (keys suits))
        [input] (get-inputs validate)
        trump (suits input)]
    trump))

(defn choose-new-kitty [hand-and-kitty]
  (print-hand-summary hand-and-kitty)
  (print "Choose new kitty")
  (let [valid (cons "" (map format-card hand-and-kitty))
        validate (validator valid (fn [in] (or ( = '("") in)
                                              (= (count in) 5))))
        inputs (get-inputs validate)
        kitty (map parse-input inputs)]
    (set kitty)))

(def responses {:rook/summary       print-initial-game-summary
                :rook/get-bid       get-bid
                :rook/get-card      get-card
                :rook/choose-kitty  choose-new-kitty
                :rook/choose-trump  choose-trump
                :rook/bid           print-bid
                :rook/trick-summary print-trick-summary
                :rook/trump-chosen  print-trump
                :rook/card-played   print-card-played
                :rook/bid-won       print-bid-won
                :rook/score         print-score })


(defrecord CliSeat [in out name]
  seat/IGoable
  (seat/in [_] in)
  (seat/out [_] out)
  (seat/dispatch [_ [topic body]]
    (let [f (or (responses topic) println)]
      (f body)))

  seat/ISeat
  (seat/display-name [_] name)

  seat/IConnected
  (seat/connected? [_] true))

(defn cli-player []
  (let [seat (->CliSeat (chan) (chan) "You")]
    (seat/go-seat seat)
    seat))
