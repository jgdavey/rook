(ns rook.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts! <! >!]]
            [goog.dom :as gdom]
            [taoensso.sente :as sente]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(declare chsk-send! set-hand! play-card-server send-bid)

(def app-state
  (atom {:players [{:count 0 :name "Me"    :bid nil}
                   {:count 0 :name "Alice" :bid nil}
                   {:count 0 :name "James" :bid nil}
                   {:count 0 :name "Mary"  :bid nil}]
         :trump nil
         :current-trick []
         :previous-trick nil
         :current-action nil ;; :get-bid, :choose-trump, :choose-kitty, :get-card
         :minimum-bid 75
         :maximux-bid 200
         :me {:hand []
              :bid nil
              :played []
              :position 0
              :name "Handsome Joe"}}))

(defn valid-bids [state]
  (let [min (:minimum-bid state)
        base (range min
                    (+ 5 (:maximux-bid state))
                    5)]
    (if (= 75 min)
      base
      (cons :pass base))))

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
        (dom/dt nil (:name player)
                    (when (:bid player)
                      (str " Bid: " (:bid player))))
        (dom/dd nil
          (apply dom/ul #js {:className "cards"}
            (map #(dom/li nil %) (range (:count player)))))))))

(defn remove-card [card]
  (fn [coll]
    (filterv (fn [c] (not (cards-equal? c card)))
             coll)))

(defn remove-cards [cards]
  (fn [coll]
    (filterv (fn [c] (not (some (partial cards-equal? c) cards)))
             coll)))

(defn deselect-card [card]
  (fn [coll]
    (mapv (fn [c] (if (cards-equal? c card)
                    (assoc card :selected nil)
                    c)) coll)))

(defn play-card [player card]
  (case (:current-action @app-state)
    :get-card (do (om/transact! player [:played] #(conj % card) :play-card)
                  (om/transact! player [:hand] (remove-card card)))
    :choose-kitty (om/transact! player [:hand] (deselect-card card))))

(defn get-rank [rank]
  (if (= rank :rook)
    "rook"
    rank))

(defn add-card-to-current-trick [card root]
  (play-card-server card)
  (om/transact! root [:current-trick] #(conj % card)))

(defn handle-selected-card [state card-path]
  (when-not (= :choose-kitty (:current-action @state))
    (let [card (get-in @state card-path)
          hand (get-in @state (pop card-path))
          card? (partial cards-equal? card)
          new-hand  (mapv (fn [c]
                            (if (card? c)
                              c
                              (assoc c :selected nil))) hand)]
      (om/update! state (pop card-path) new-hand))))

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

(defn kitty-view [hand owner]
  (reify om/IRender
    (render [_]
      (dom/div #js {:className "modal"}
        (dom/p nil "Choose kitty")
        (when (= 5 (count (filter :selected hand)))
          (dom/button #js {:onClick #(send-kitty @hand)} "Ok"))))))

(defn trump-view [_ owner]
  (reify om/IRender
    (render [_]
      (dom/div #js {:className "modal"}
        (dom/p nil "Choose trump")
        (apply dom/div nil
               (map (fn [[_ suit]]
                      (dom/button #js {:onClick #(send-trump suit)}
                                  (name suit))) suits))))))

(defn bids-view [state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:bid-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :bid-chan)]
        (go (loop []
              (when-let [v (<! c)]
                (send-bid v)
                (recur))))))
    om/IRenderState
    (render-state [_ {:keys [bid-chan]}]
      (apply dom/div #js {:className "modal"}
             (map (fn [amount]
                    (let [label (if (keyword? amount) (name amount) amount)]
                      (dom/button #js {:onClick (fn [_] (put! bid-chan amount))
                                       :className (when (keyword? amount) (name amount))}
                                label))) (valid-bids state))))))

(defn app-view [state owner]
  (reify
    om/IRender
    (render [_]
      (let [pos (positions state)
            action (:current-action state)
            my-hand (get-in state [:me :hand])
            trick-opts {:relative-position (get-in state [:me :position])}]
        (dom/div nil
          (dom/h1 nil "Rook")
          (when-let [trump (:trump state)]
            (dom/p nil (str "Trump: " (get-suit trump))))
          (when-let [action (:current-action state)]
            (dom/p nil (str "Current Action: " (name action))))
          (when (= :get-bid action)
            (om/build bids-view state))
          (when (= :choose-kitty action)
            (om/build kitty-view my-hand))
          (when (= :choose-trump action)
            (om/build trump-view state))
          (om/build opponent-view (:west pos) {:opts {:list "west"}})
          (om/build opponent-view (:north pos) {:opts {:list "north"}})
          (om/build opponent-view (:east pos) {:opts {:list "east"}})
          (om/build hand-view (:me state))
          (om/build trick-view (:current-trick state) {:opts trick-opts})
          (om/build previous-trick-view (:previous-trick state) {:opts trick-opts}))))))

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

(def suits { "B" :black, "R" :red, "Y" :yellow, "G" :green})

(def ranks
  (apply hash-map
    (->> (vec (range 1 15))
         (mapcat (fn [r] [(str r) r]))
         (list* "R" :rook))))

(defn send-bid [bid]
  (swap! app-state update-in [:me :bid] (constantly bid))
  (chsk-send! [:rook/bid bid]))

(defn send-kitty [hand]
  (let [kitty (filter :selected hand)]
    (swap! app-state update-in [:kitty] (constantly kitty))
    (swap! app-state update-in [:me :hand] (remove-cards kitty))
    (chsk-send! [:rook/kitty kitty])))

(defn send-trump [trump]
  (let [t (if (string? trump)
            (suits trump)
            trump)]
    (chsk-send! [:rook/trump t])))

(defn get-bid [{:keys [bids current-bid] :as bid-status}]
  (swap! app-state assoc :current-action :get-bid)
  (swap! app-state assoc :minimum-bid (+ 5 current-bid))
  (doseq [bid bids]
    (swap! app-state update-in [:players (:seat bid) :bid] (constantly (:bid bid))))
  (println "Get a bid. Status:" bid-status))

(defn get-card [{:keys [hand legal-moves trick led]}]
  (swap! app-state assoc :current-action :get-card)
  (swap! app-state assoc :current-trick trick)
  (set-hand! hand))

(defn play-card-server [card]
  (swap! app-state assoc :current-action nil)
  (chsk-send! [:rook/card card]))

(def ^:private sorter #(vector (:suit %) (:value %)))

(defn sorted-hand [cards]
  (some->> cards (sort-by sorter) (mapv #(assoc % :selected nil))))

(defn set-hand! [cards]
  (if-let [hand (sorted-hand cards)]
    (swap! app-state update-in [:me :hand] (constantly hand))))

(defn set-trump! [trump]
  (when trump
    (swap! app-state assoc :trump trump)))

(defn set-previous-trick! [new-val]
  (let [pset! #(swap! app-state assoc :previous-trick %)]
    (pset! nil)
    (js/setTimeout #(pset! (dissoc new-val :winning-position)) 17)
    (js/setTimeout #(pset! new-val) 150)))

(defn set-trick! [trick]
  (when trick
    (swap! app-state assoc :current-trick trick)))

(defn set-position! [position]
  (when position
    (swap! app-state update-in [:me :position] (constantly position))))

(defn set-card-played! [data]
  (swap! app-state assoc :current-action nil)
  (swap! app-state update-in [:current-trick] conj (last data)))

(defn set-bid! [[position bid :as message]]
  (swap! app-state assoc :current-action nil)
  (swap! app-state update-in [:players position :bid] (constantly bid)))

(defn set-players! [players]
  (println players)
  (doall
    (map-indexed (fn [i player]
                   (swap! app-state update-in [:players i] merge player)) players)))

(defn choose-trump [status]
  (swap! app-state assoc :current-action :choose-trump)
  (println "Choose trump. Status:" status))

(defn choose-kitty [hand-and-kitty]
  (swap! app-state assoc :current-action :choose-kitty)
  (set-hand! hand-and-kitty))

(defn parse-status [{:keys [hand players trump position trick] :as status}]
  (set-hand! hand)
  (set-trump! trump)
  (set-players! players)
  (set-position! position)
  (set-trick! trick))

(defn- recv [[id data]]
  (case id
    :rook/get-bid (get-bid data)
    :rook/get-card (get-card data)
    :rook/choose-kitty (choose-kitty data)
    :rook/choose-trump (choose-trump data)
    :rook/status (parse-status data)
    :rook/bid (set-bid! data)
    :rook/trump-chosen (set-trump! data)
    :rook/trick-summary (set-previous-trick! data)
    :rook/card-played (set-card-played! data)
    ;; else
    (println id data)))

(defn- event-handler [[id data] _]
  (case id
    :chsk/state println
    :chsk/recv (recv data)))

(defonce chsk-router
  (sente/start-chsk-router-loop! event-handler ch-chsk))
