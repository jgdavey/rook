(ns rook.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts! <! >!]]
            [goog.dom :as gdom]
            [taoensso.sente :as sente]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(declare chsk-send! set-hand! play-card-server)

(def app-state
  (atom {:players [{:count 14 :name "Me"}
                   {:count 14 :name "Alice"}
                   {:count 14 :name "James"}
                   {:count 14 :name "Mary"}]
         :trump nil
         :current-trick []
         :previous-trick nil
         :me {:hand []
              :played []
              :position 0
              :name "Handsome Joe"}}))

(defn cards-equal? [card1 card2]
  (and (= (:rank card1) (:rank card2))
       (= (:suit card1) (:suit card2))))

(defn dir-relative [south n]
  (get [:south :west :north :east]
       (mod (- n south) 4)))

(defn positions [state]
  (let [my-position (get-in state [:me :position])
        directions (reduce (fn [all i]
                             (assoc all
                                    (dir-relative my-position i)
                                    (get-in state [:players i]))) {} (range 4))]
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
  (when (:trump @app-state)
    (om/transact! player [:played] #(conj % card) :play-card)
    (om/transact! player [:hand] (remove-card card))))

(defn get-rank [rank]
  (if (= rank :rook)
    "rook"
    rank))

(defn add-card-to-current-trick [card root]
  (play-card-server card)
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
  (when suit (name suit)))

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
            class-name (apply str (interpose " " classes))
            points* (:points card)
            points (when (and points* (not (zero? points*))) points*) ]
        (dom/li #js {:className class-name
                     :onClick #(if selected
                                 (when choose-card-chan (put! choose-card-chan card))
                                 (om/update! card :selected true :select-card))}
                (dom/em #js {:className "rank"} rank)
                (dom/span #js {:className "suit"} suit)
                (dom/div #js {:className "rank"} (dom/span nil (get-label (:rank card))))
                (dom/div #js {:className "points"} points))))))

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

(defn previous-trick-view [previous owner {:keys [relative-position]}]
  (reify
    om/IRender
    (render [_]
      (let [winner (:winning-position previous)
            dir (when-not (nil? winner) (dir-relative relative-position winner))
            cards (get previous :trick [])
            css-class "previous_trick cards"
            css-class (if dir (str css-class " animate-" (name dir)) css-class)]
        (apply dom/ul #js {:className css-class}
               (map (fn [c]
                      (om/build card-view c {:opts
                                             {:direction
                                              (when-let [s (dir-relative relative-position (:seat c))]
                                                (name s))}})) cards))))))

(defn app-view [state owner]
  (reify
    om/IRender
    (render [_]
      (let [pos (positions state)
            trick-opts {:relative-position (get-in state [:me :position])}]
        (dom/div nil
          (dom/h1 nil "Rook")
          (when-let [trump (:trump state)]
            (dom/p nil (str "Trump: " (get-suit trump))))
          (om/build opponent-view (:west pos) {:opts {:list "west"}})
          (om/build opponent-view (:north pos) {:opts {:list "north"}})
          (om/build opponent-view (:east pos) {:opts {:list "east"}})
          (om/build hand-view (:me state))
          (om/build trick-view (:current-trick state) {:opts trick-opts})
          (om/build previous-trick-view (:previous-trick state) {:opts trick-opts }))))))

(om/root app-view app-state
         {:target (gdom/getElement "board")
          :tx-listen (fn [m root-state]
                       (let [{:keys [old-state new-state path old-value new-value tag]} m]
                         (case tag
                           :select-card (when new-value (handle-selected-card root-state (pop path)))
                           :play-card (add-card-to-current-trick (peek new-value) root-state)
                           nil)))})

(let [url (str (.. js/window -location -pathname) "/chsk")
      {:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket! url {:type :auto})]
  (def chsk       chsk)
  (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def chsk-state state))

(defn send-bid [bid]
  (chsk-send! [:rook/bid bid]))

(defn get-bid [bid-status]
  (println "Get a bid. Status:" bid-status))

(defn get-card [{:keys [hand legal-moves trick led]}]
  (swap! app-state assoc :current-trick trick)
  (set-hand! hand))

(defn play-card-server [card]
  (chsk-send! [:rook/card card]))

(def suits { "B" :black, "R" :red, "Y" :yellow, "G" :green})
(def ranks
  (apply hash-map
    (->> (vec (range 1 15))
         (mapcat (fn [r] [(str r) r]))
         (list* "R" :rook))))

(def ^:private sorter #(vector (:suit %) (:value %)))

(defn sorted-hand [cards]
  (some->> cards (sort-by sorter) (mapv #(assoc % :selected nil))))

(defn set-hand! [cards]
  (if-let [hand (sorted-hand cards)]
    (swap! app-state update-in [:me :hand] (constantly hand))))

(defn set-trump! [trump]
  (println "Trump is" trump)
  (when trump
    (swap! app-state assoc :trump trump)))

(defn set-previous-trick! [new-val]
  (let [pset! #(swap! app-state assoc :previous-trick %)]
    (pset! nil)
    (js/setTimeout #(pset! (dissoc new-val :winning-position)) 17)
    (js/setTimeout #(pset! new-val) 50)))

(defn set-trick! [trick]
  (when trick
    (swap! app-state assoc :current-trick trick)))

(defn set-position! [position]
  (when position
    (swap! app-state update-in [:me :position] (constantly position))))

(defn- parse-ascii-input [string]
  (when-let [matches (re-find #"^(\d{1,2}|R)([BGRY])" string)]
    (let [[_ rank suit] matches]
      {:suit (suits suit)
       :rank (ranks rank)})))

(defn play-card-ascii [string]
  (chsk-send! [:rook/card (parse-ascii-input string)]))

(defn choose-trump [status]
  (println "Choose trump. Status:" status))

(defn choose-kitty [status]
  (println "Choose kitty. Status:" status))

(defn parse-status [{:keys [hand trump position trick]}]
  (set-hand! hand)
  (set-trump! trump)
  (set-position! position)
  (set-trick! trick))

(defn- recv [[id data]]
  (case id
    :rook/get-bid (get-bid data)
    :rook/get-card (get-card data)
    :rook/choose-kitty (choose-kitty data)
    :rook/choose-trump (choose-trump data)
    :rook/status (parse-status data)
    :rook/trump-chosen (set-trump! (first data))
    :rook/trick-summary (set-previous-trick! (first data))
    :rook/card-played (swap! app-state update-in [:current-trick] conj (last data))
    ;; else
    (println id data)))

(defn- event-handler [[id data] _]
  (case id
    :chsk/state println
    :chsk/recv (recv data)))

(defonce chsk-router
  (sente/start-chsk-router-loop! event-handler ch-chsk))
