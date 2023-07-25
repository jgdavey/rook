(ns rook.mcts
  (:require
   [clojure.set :as set]
   [rook.game :as g]))

(def ^:const exploration-constant 0.9)

(defn ucb [parent {:keys [n t] :as node}]
  (if (zero? n)
    ##Inf
    (let [top (or parent node)]
      (+
       (/ t n)
       (* exploration-constant
          (Math/sqrt (/ (Math/log (:n top))
                        n)))))))

(defn make-node [game action]
  {:action action
   :children nil
   :t 0
   :n 0
   :game game})

(defn make-children [node game actions]
  (when-not (g/game-over? game)
    (let [seat (g/next-seat game)]
      (assoc node
             :children
             (into []
                   (map (fn [action]
                          (make-node (g/play* game seat action) action)))
                   actions)))))

(defn shuffled-next-actions [game seat]
  (let [next-seat (g/next-seat game)]
    (seq
     (if (= next-seat seat)
       (some-> (g/status game) :legal-moves seq shuffle)
       (g/potential-opponent-cards game seat next-seat)))))

(defn make-root
  "Because this is from the perspective of a single seat, remove other
  seats and lump into a available cards"
  [game]
  (-> (make-node game nil)
      (make-children game (:legal-moves (g/status game)))))

(defn backpropagate [tree path score]
  (let [paths (into [] (comp (take (count path))
                             (remove #(= :children (peek %))))
                    (iterate pop path))]
    (reduce (fn [t path]
              (update-in t path
                         #(-> %
                              (update :t + score)
                              (update :n inc))))
            (update tree :n inc)
            paths)))

(defn playout [base-game seat]
  (loop [game base-game]
    (if (g/game-over? game)
      game
      (let [actions (shuffled-next-actions game seat)
            next-seat (g/next-seat game)]
        (if (not (pos? (count actions)))
          (do
            (def next-seat next-seat)
            (def game game)
            (throw (ex-info "Ouch" {})))
          (do
            (def prev-seat next-seat)
            (def prev-game game)
            (recur
             (g/play game next-seat (first actions)))))))))

(defn monte-carlo-step [tree]
  (let [max-depth (or (:max-tree-depth (meta tree)) 99999)
        max-width (or (:max-tree-width (meta tree)) 99999)
        seat (g/next-seat (get-in tree [:game]))
        team (mod seat 2)
        ;; select
        [depth path node] (loop [depth 0
                                 p []
                                 node tree]
                            (if (nil? (:children node))
                              [depth p node]
                              (let [[idx child-node] (apply max-key #(ucb node (peek %))
                                                            (shuffle (map-indexed vector (:children node))))]
                                (recur (inc depth) (into p [:children idx]) child-node))))
        expand? (and (pos? (:n node))
                     (< depth max-depth))
        ;; expand
        [tree path node] (if expand?
                           (let [new-tree (update-in tree path
                                                     make-children
                                                     (:game node)
                                                     (take max-width
                                                           (shuffled-next-actions (:game node) seat)))
                                 children (get-in new-tree (conj path :children))
                                 new-path (into path [:children 0])]
                             (if (pos? (count children))
                               [new-tree new-path (first children)]
                               [tree path node]))
                           [tree path node])
        result (playout (:game node) seat)]
    (backpropagate tree path (get-in (g/score result) [team :score]))))

(defn choose-next-card [game {:keys [iterations] :as config}]
  (let [nexts (shuffled-next-actions game (g/next-seat game))]
    (if (<= (count nexts) 1)
      (first nexts)
      (let [tree (loop [i (or iterations 1)
                        tree (with-meta (make-root (dissoc game :players :bids))
                               config)]
                   (if (pos? i)
                     (recur (dec i) (monte-carlo-step tree))
                     tree))
            chosen (apply max-key #(/ (:t %) (:n %)) (:children tree))]
        (:action chosen)))))
