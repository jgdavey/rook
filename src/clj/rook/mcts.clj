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

(defn next-actions [game seat]
  (seq
   (if (= (g/next-seat game) seat)
     (:legal-moves (g/status game))
     (get-in game [:seats seat :opponent-cards]))))

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

(defn playout [game seat]
  (loop [g game]
    (if (g/game-over? g)
      g
      (recur
       (g/play* g (g/next-seat g) (rand-nth (next-actions g seat)))))))

(defn monte-carlo-step [tree]
  (let [max-depth (or (:max-tree-depth (meta tree)) 99999)
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
                                                     (next-actions (:game node) seat))
                                 children (get-in new-tree (conj path :children))
                                 n (rand-int (count children))
                                 new-path (into path [:children n])]
                             (if (> (count children) 1)
                               [new-tree new-path (nth children n)]
                               [tree path node]))
                           [tree path node])
        result (playout (:game node) seat)
        score (get-in (g/score result) [team :score])]
    (backpropagate tree path score)))

(defn choose-next-card [game {:keys [iterations max-tree-depth]}]
  (when-let [nexts (next-actions game (g/next-seat game))]
    (if (= 1 (count nexts))
      (first nexts)
      (let [tree (loop [i (or iterations 1)
                        tree (with-meta (make-root (dissoc game :players :bids))
                               {:max-tree-depth max-tree-depth})]
                   (if (pos? i)
                     (recur (dec i) (monte-carlo-step tree))
                     tree))
            chosen (apply max-key #(/ (:t %) (:n %)) (:children tree))]
        (:action chosen)))))
