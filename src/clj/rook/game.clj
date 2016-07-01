(ns rook.game
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.set :as set]))

(def rook? #{[:rook :rook]})
(def suit? #{:red :green :black :yellow})
(def deck (into rook?
                (for [suit suit?
                      rank (range 1 15)]
                                 [rank suit])))

(s/def ::card deck)
(s/def ::hand (s/coll-of ::card :distinct true))
(s/def ::deck (s/coll-of ::card))
(s/def ::kitty (s/coll-of ::card :distinct true))
(s/def ::name string?)

(s/def ::player  (s/keys :req  [::name ::hand]))

(s/def ::players (s/coll-of ::player :min-count 2 :max-count 6))

(s/def ::kitty-size integer?)
(s/def ::rules (s/keys :req [::kitty-size]))
(s/def ::game (s/keys :req [::players ::deck ::kitty ::rules]))

(defn all-cards [{:keys [::deck ::kitty ::players] :as game}]
  (apply concat deck kitty (map ::hand players)))

(defn strictly-set-equal? [& colls]
  (and (apply = (map count colls))
       (apply = (map set colls))))

(defn valid-initial-game? [{:keys [::deck ::kitty ::players] :as game}]
  (apply = (map (comp count ::hand) players)))

(s/fdef deal
  :args (s/cat :game ::game)
  :ret ::game
  :fn #(and (strictly-set-equal? (all-cards (-> % :args :game))
                                  (all-cards (-> % :ret)))
            (valid-initial-game? (-> % :ret))))

(defn deal [{deck ::deck, players ::players, {:keys [::kitty-size]} ::rules, :as game}]
  (let [[kitty player-cards] (split-at kitty-size (shuffle deck))
        shuffled (apply (partial mapv vector) (partition (count players) player-cards))]
    (assoc game
           ::players (mapv #(assoc %1 ::hand (vec %2)) players shuffled)
           ::deck []
           ::kitty (set kitty))))

(comment

(clojure.spec.test/instrument `deal)
(clojure.spec.test/test `deal)
(clojure.spec.gen/sample (s/gen ::players))
(clojure.spec.gen/sample (s/gen ::deck) 1)



(def init-game {::players [{::name "A" ::hand []}
                           {::name "B" ::hand []}
                           {::name "C" ::hand []}
                           {::name "D" ::hand []}]
                ::kitty []
                ::rules {::kitty-size 5}
                ::deck deck})
(deal init-game)

)
