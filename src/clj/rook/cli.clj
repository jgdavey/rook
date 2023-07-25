(ns rook.cli
  (:require
   [clojure.string :as str :refer [trim-newline]]
   [clojure.core.async :as async :refer [chan]]
   [rook.seat :as seat]
   [rook.game :as game]
   [rook.util :as util :refer [display-card display-cards]]))

(def suits
  (reduce-kv (fn [m k v] (assoc m v k)) {} util/labels))

(def ranks
  (apply hash-map
    (->> game/ranks
         (mapcat (fn [r] [(str r) r]))
         (cons :rook)
         (cons "R"))))

(defmulti dispatch (fn [[topic body] context] topic))

(defn print-trick-status [{:keys [trick hand legal-moves]}]
  (println
   "\nPlayed so far: "
   (mapv display-card trick)
   "\nYour hand:     "
   (util/display-cards-with-legal hand legal-moves)))

(defn print-hand-summary [hand]
  (println "Your hand:" (display-cards hand)))

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
    (let [raw (trim-newline (or (read-line) "quit"))
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

(defmethod dispatch :rook/status [[_ {:keys [hand] :as c}] context]
  (println)
  (println "*** Welcome to Rook ***")
  (println)
  (reset! context (dissoc c :hand))
  (print-hand-summary hand))

(defmethod dispatch :rook/score [[_ score] context]
  (doseq [team (sort-by :position score)
          :let [players (map #(get-in @context [:players % :name])
                             (:members team))]]
    (println (str/join " and " players)
             (str "[team " (:position team) "]")
             "scored"
             (:score team))))

(defmethod dispatch :rook/trick-summary [[_ trick-summary] context]
  (let [{:keys [trick winning-card winning-team winning-position]} trick-summary
        team-name (str "[team " (:position winning-team) "]")
        player (get-in @context [:players winning-position :name])]
    (print "\nTrick: ")
    (println (mapv display-card trick))
    (println player team-name "wins trick with" (display-card winning-card))
    (println)))

(defmethod dispatch :rook/card-played [[_ [player card]] context]
  (let [player (get-in @context [:players player :name] player)]
    (println (format "%14s" player) "played" (display-card card))))

(defmethod dispatch :rook/bid-won [[_ [player bid]] context]
  (let [player (get-in @context [:players player :name] player)]
    (println (format "%14s" player) "took the bid at" bid)
    (println)))

(defmethod dispatch :rook/bid [[_ [player bid]] context]
  (let [player (get-in @context [:players player :name] player)
        message (if (and bid (not= :pass bid)) (str "bid " bid) "passed")]
    (println (format "%14s" player) message)))

(defmethod dispatch :rook/trump-chosen [[_ trump] _]
  (println "Trump is" (util/colorize (name trump) trump)))

(defmethod dispatch :rook/get-card [[_ {:keys [legal-moves] :as status}] _]
  (print-trick-status status)
  (let [valid (cons "quit" (map util/format-card legal-moves))
        validate (validator valid)
        [input] (get-inputs validate)]
    (parse-input input)))

(defmethod dispatch :rook/get-bid [[_ status] _]
  (print "Your bid")
  (let [{:keys [current-bid]} status
        valid (cons "pass" (map str (range (+ 5 current-bid) 205 5)))
        validate (validator valid)
        [input] (get-inputs validate)]
    (parse-bid input)))

(defmethod dispatch :rook/choose-trump [[_ _] _]
  (print "Choose trump")
  (let [validate (validator (keys suits))
        [input] (get-inputs validate)
        trump (suits input)]
    trump))

(defmethod dispatch :rook/choose-kitty [[_ hand-and-kitty] _]
  (print-hand-summary hand-and-kitty)
  (print "Choose new kitty")
  (let [valid (cons "" (map util/format-card hand-and-kitty))
        validate (validator valid (fn [in] (or (= '("") in)
                                               (= (count in) 5))))
        inputs (get-inputs validate)
        kitty (map parse-input inputs)]
    (set kitty)))

(defrecord CliSeat [in out name context]
  seat/IGoable
  (seat/in [_] in)
  (seat/out [_] out)
  (seat/dispatch [_ message]
    (dispatch message context))

  seat/ISeat
  (seat/display-name [_] name)

  seat/IConnected
  (seat/connected? [_] true))

(defn cli-player []
  (let [seat (->CliSeat (chan) (chan) "You" (atom {}))]
    (seat/go-seat seat)
    seat))
