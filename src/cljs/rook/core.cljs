(ns rook.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts!]]
            [goog.dom :as gdom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:players [{:count 12 :name "Alice"}
                   {:count 12 :name "Me"}
                   {:count 11 :name "James"}
                   {:count 12 :name "Mary"}]
         :me {:hand #{{:suit :green
                       :rank 5
                       :value 5
                       :points 5}
                      {:suit :green
                       :rank 7
                       :value 7}
                      {:suit :green
                       :rank 12
                       :value 12}
                      {:suit :green
                       :rank 13
                       :value 13}
                      {:suit :black
                       :rank 10
                       :value 10
                       :points 10}
                      {:suit :black
                       :rank 11
                       :value 11}
                      {:suit :red
                       :rank :rook
                       :value 0
                       :points 20}
                      {:suit :red
                       :rank 14
                       :value 14
                       :points 10}
                      {:suit :red
                       :rank 1
                       :value 15
                       :points 15}
                      {:suit :yellow
                       :rank 6
                       :value 6}
                      {:suit :yellow
                       :rank 7
                       :value 7}}
              :position 1
              :name "Handsome Joe"}}))

(defn positions [state]
  (let [my-position (get-in state [:me :position])
        [west north east] (map #(mod % 4) (range (inc my-position) (+ 4 my-position)))]
    {:north (get-in state [:players north])
     :east (get-in state [:players east])
     :west (get-in state [:players west])}))

(defn opponent-view [player owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/dl #js {:className (:list opts) }
        (dom/dt nil (:name player))
        (dom/dd nil
          (apply dom/ul #js {:className "cards"}
            (map #(dom/li nil %) (range (:count player)))))))))

(defn get-rank [rank]
  (if (= rank :rook)
    "rook"
    rank))

(defn get-label [rank]
  (if (= rank :rook)
    "R"
    rank))

(defn get-suit [suit]
  (name suit))

(defn card-view [card owner]
  (reify
    om/IRender
    (render [_]
      (let [suit (get-suit (:suit card))
            rank (get-rank (:rank card))]
        (dom/li #js {:className (str suit " rank-" rank)}
                (dom/em #js {:className "rank"} rank)
                (dom/span #js {:className "suit"} suit)
                (dom/div #js {:className "rank"} (dom/span nil (get-label (:rank card))))
                (dom/div #js {:className "points"} (:points card)))))))

(defn sorted-hand [hand]
  (let [sorter #(vector (:suit %) (:value %))]
    (->> hand
         (sort-by sorter))))

(defn hand-view [player owner]
  (reify
    om/IRender
    (render [_]
      (dom/dl #js {:className "hand south" }
        (dom/dt nil (:name player))
        (dom/dd nil
          (apply dom/ul #js {:className "cards"}
            (om/build-all card-view (sorted-hand (:hand player)))))))))


(defn app-view [state owner]
  (reify
    om/IRender
    (render [_]
      (let [pos (positions state)]
        (dom/div nil
          (dom/h1 nil "Rook")
          (om/build opponent-view (:west pos) {:opts {:list "west"}})
          (om/build opponent-view (:north pos) {:opts {:list "north"}})
          (om/build opponent-view (:east pos) {:opts {:list "east"}})
          (om/build hand-view (:me state)))))))


(om/root app-view app-state
         {:target (gdom/getElement "board")})
