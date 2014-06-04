(ns rook.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts! <! >!]]
            [goog.dom :as gdom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:players [{:count 12 :name "Alice"}
                   {:count 12 :name "Me"}
                   {:count 11 :name "James"}
                   {:count 12 :name "Mary"}]
         :current-trick [{:suit :green :rank 5 :value 5 :points 5 :seat 2}]
         :me {:hand [{:suit :green
                      :seat 1
                      :rank 5
                      :value 5
                      :points 5}
                     {:suit :green
                      :seat 1
                      :rank 7
                      :value 7}
                     {:suit :green
                      :seat 1
                      :rank 12
                      :value 12}
                     {:suit :green
                      :seat 1
                      :rank 13
                      :value 13}
                     {:suit :black
                      :seat 1
                      :rank 10
                      :value 10
                      :points 10}
                     {:suit :black
                      :seat 1
                      :rank 11
                      :value 11}
                     {:suit :red
                      :seat 1
                      :rank :rook
                      :value 0
                      :points 20}
                     {:suit :red
                      :seat 1
                      :rank 14
                      :value 14
                      :points 10}
                     {:suit :red
                      :seat 1
                      :rank 1
                      :value 15
                      :points 15}
                     {:suit :yellow
                      :seat 1
                      :rank 6
                      :value 6}
                     {:suit :yellow
                      :seat 1
                      :rank 7
                      :value 7}]
              :played []
              :position 1
              :name "Handsome Joe"}}))

(defn cards-equal? [card1 card2]
  (and (= (:rank card1) (:rank card2))
       (= (:suit card1) (:suit card2))))

(defn dir-relative [south n]
  (get [:south :west :north :east]
       (mod (- n south ) 4)))

(defn positions [state]
  (let [my-position (get-in state [:me :position])
        directions (reduce (fn [all i]
                             (assoc all
                                    (dir-relative my-position i)
                                    (get-in state [:players i]))) {} (range 4))]
    (println directions)
    directions))

(defn opponent-view [player owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/dl #js {:className (:list opts) }
        (dom/dt nil (:name player))
        (dom/dd nil
          (apply dom/ul #js {:className "cards"}
            (map #(dom/li nil %) (range (:count player)))))))))

(defn remove-card [card]
  (fn [coll]
    (filterv (fn [c] (not (cards-equal? c card)))
             coll)))

(defn play-card [player card]
  (om/transact! player [:played] #(conj % card) :play-card)
  (om/transact! player [:hand] (remove-card card)))

(defn get-rank [rank]
  (if (= rank :rook)
    "rook"
    rank))

(defn add-card-to-current-trick [card root]
  (om/transact! root [:current-trick] #(conj % card)))

(defn handle-selected-card [state card-path]
  (let [card (get-in @state card-path)
        hand (get-in @state (pop card-path))
        card? (partial cards-equal? card)
        new-hand  (mapv (fn [c]
                          (if (card? c)
                            c
                            (assoc c :selected nil))) hand)]
    (om/update! state (pop card-path) new-hand)))

(defn get-label [rank]
  (if (= rank :rook)
    "R"
    rank))

(defn get-suit [suit]
  (name suit))

(defn card-view [card owner {:keys [direction]}]
  (reify
    om/IRenderState
    (render-state [_ {:keys [choose-card-chan]}]
      (let [{:keys [suit rank selected seat]} card
            suit (get-suit suit)
            rank (get-rank rank)
            classes [suit (str "rank-" rank)]
            classes (if selected (conj classes "selected") classes)
            classes (if direction (conj classes direction) classes)
            class-name (apply str (interpose " " classes))]
        (dom/li #js {:className class-name
                     :onClick #(if selected
                                 (when choose-card-chan (put! choose-card-chan card))
                                 (om/update! card :selected true :select-card))}
                (dom/em #js {:className "rank"} rank)
                (dom/span #js {:className "suit"} suit)
                (dom/div #js {:className "rank"} (dom/span nil (get-label (:rank card))))
                (dom/div #js {:className "points"} (:points card)))))))

(defn hand-view [player owner]
  (reify
    om/IInitState
    (init-state [_]
      {:choose-card-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :choose-card-chan)]
        (go (loop []
              (when-let [card (<! c)]
                (play-card player @card)
                (recur))))))
    om/IRender
    (render [_]
      (dom/dl #js {:className "hand south" }
        (dom/dt nil (:name player))
        (dom/dd nil
          (apply dom/ul #js {:className "cards"}
            (om/build-all card-view (:hand player) {:init-state
                                                     {:choose-card-chan (om/get-state owner :choose-card-chan)}})))))))

(defn trick-view [trick owner {:keys [relative-position]}]
  (reify
    om/IRender
    (render [_]
      (apply dom/ul #js {:className "current_trick cards"}
             (map (fn [c]
                    (om/build card-view c {:opts
                                           {:direction
                                            (when-let [s (dir-relative relative-position (:seat c))]
                                              (name s))}})) trick)))))


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
          (om/build hand-view (:me state))
          (om/build trick-view (:current-trick state) {:opts {:relative-position (get-in state [:me :position])}}))))))

(om/root app-view app-state
         {:target (gdom/getElement "board")
          :tx-listen (fn [m root-state]
                       (let [{:keys [old-state new-state path old-value new-value tag]} m]
                         (case tag
                           :select-card (when new-value (handle-selected-card root-state (pop path)))
                           :play-card (add-card-to-current-trick (peek new-value) root-state)
                           nil)))})
