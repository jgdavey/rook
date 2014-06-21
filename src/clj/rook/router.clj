(ns rook.router
  (:require [com.stuartsierra.component :as component]
            [ring.util.response :as res :refer [file-response]]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.session :as session]
            [compojure.core :refer [routes GET POST]]
            [compojure.route :as route]
            [rook.engine :as engine]
            [rook.protocols :refer :all]
            [rook.repository :as repo :refer [find-game add-game]]
            [clojure.core.async :as async :refer [go >! <! put! <!! >!! chan sub close!]]
            [taoensso.sente :as sente]))

(defonce game-loops (atom []))

(defrecord Player [id c send! events]
  IPlayer
  (display-name [_] (str id))
  (summarize [_ status]
    (send! id [:rook/status status]))
  (get-card [_ status]
    (send! id [:rook/get-card status])
    (let [t (async/timeout 20000)
          [val port] (async/alts!! [c t])]
      (if (identical? t port)
        :timeout
        val)))
  (get-bid [_ bid-status]
    (send! id [:rook/get-bid bid-status])
    (let [val (<!! c)]
      (when (number? val) val)))
  (choose-new-kitty [_ hand-and-kitty]
    (send! id [:rook/choose-kitty hand-and-kitty])
    (<!! c))
  (choose-trump [_ hand]
    (send! id [:rook/choose-trump hand])
    (<!! c)))

(defn new-player [id pub send-fn]
  (let [event-sub (chan)
        player (map->Player {:id id :c (chan) :send! send-fn :events event-sub})]
    (sub pub :all event-sub)
    (go (loop []
          (when-some [msg (<! event-sub)]
            (let [[topic & data] msg
                  topic (keyword "rook" (name topic))]
              (send-fn id [topic data]))
            (recur))))
    player))

(defn game []
  (let [g (atom nil)]
    (engine/setup-web-game g)
    (let [f (future (engine/web-game-loop g))]
      (swap! game-loops conj f))
    g))

(defn login! [{:keys [session params]}]
  (let [login (get params "login")]
    (assoc (res/redirect-after-post "/game")
           :session (assoc session :uid login))))

(defn index []
  (file-response "public/login.html" {:root "resources"}))

(defn new-game [{:keys [params session cookies]}]
  (file-response "public/newgame.html" {:root "resources"}))

(defn create-game [repository]
  (let [id (add-game repository (game))]
    (assert id)
    (res/redirect-after-post (str "/games/" id))))

(defn connect! [game player-id send-fn]
  (let [pub (:pub-chan @game)
        player (new-player player-id pub send-fn)]
    (engine/connect-player game player)))

(defn disconnect! [game player-id]
  (let [player (engine/find-player-with-id game player-id)]
    (when-let [c (:events player)]
      (close! c))
    (when-let [c (:c player)]
      (>!! c :timeout))
    (engine/disconnect-player game player-id)))

(defn forward [player data]
  (put! (:c player) data))

(defn- make-event-handler [repository {:keys [send-fn] :as sente}]
  (fn [{:keys [ring-req event ?reply-fn] :as ev-msg } _]
    (let [player-id (get-in ring-req [:session :uid])
          game-id (get-in ring-req [:route-params :uuid])
          game (find-game repository game-id)
          position (engine/position-of-player-id game player-id)
          player (get-in @game [:players position])
          [event-id data :as ev] event]
      (case event-id
        :chsk/uidport-open (connect! game player-id send-fn)
        :chsk/uidport-close (disconnect! game player-id)
        :chsk/ping nil
        :rook/bid (forward player data)
        :rook/card (forward player data)
        :rook/kitty (forward player data)
        :rook/trump (forward player data)
        ;; else
        nil))))

(defn make-routes [repository {:keys [ch-recv send-fn
                                      ajax-post-fn ajax-get-or-ws-handshake-fn
                                      connected-uids] :as sente}]
  (routes
    (GET "/" [] (index))
    (POST "/login" req (login! req))
    (GET "/game" req (new-game req))
    (POST "/games" [] (create-game repository))
    (GET  "/games/:uuid/chsk" req (ajax-get-or-ws-handshake-fn req))
    (POST "/games/:uuid/chsk" req (ajax-post-fn                req))
    (GET  "/games/:uuid" [uuid]
      (if-let [game (find-game repository uuid)]
         (file-response "public/dev.html" {:root "resources"})
         (res/not-found "No game with that id")))
    (route/files "/" {:root "resources/public"})))

(defrecord Router [repository handler stop-fn]
  component/Lifecycle

  (start [router]
    (let [socket (sente/make-channel-socket! {})
          dispatch (make-event-handler repository socket)
          stop-fn (sente/start-chsk-router-loop! dispatch (:ch-recv socket))]
      (-> router
          (assoc :stop-fn stop-fn)
          (assoc :handler (-> (make-routes repository socket)
                              session/wrap-session
                              cookies/wrap-cookies
                              params/wrap-params)))))

  (stop [router]
    (doseq [fut @game-loops]
      (future-cancel fut))
    (reset! game-loops [])
    (when stop-fn (stop-fn))
    (-> router
        (dissoc :stop-fn)
        (dissoc :handler))))

(defn new-router []
  (->Router nil nil nil))
