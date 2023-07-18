(ns rook.router
  (:require [com.stuartsierra.component :as component]
            [ring.util.response :as res :refer [file-response]]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.session :as session]
            [compojure.core :refer [routes GET POST]]
            [compojure.route :as route]
            [rook.engine :as engine]
            [rook.seat :as seat]
            [rook.repository :as repo :refer [find-game add-game]]
            [clojure.core.async :as async :refer [go >! <! put! <!! >!! chan sub close!]]
            [taoensso.sente :as sente]))

(defonce game-loops (atom []))

(defrecord Player [in out id send!]
  seat/IGoable
  (seat/in [_] in)
  (seat/out [_] out)
  (seat/dispatch [_ [topic body]]
    (send! [topic (if (= :rook/get-card topic)
                    (dissoc body :game)
                    body)])
    nil)

  seat/ISeat
  (seat/display-name [_] (str id))

  seat/IConnected
  (seat/connected? [_] true))


(defn new-player [id send-fn]
  (let [player (map->Player {:in (chan)
                             :out (chan)
                             :id id
                             :send! (partial send-fn id)})]
    (seat/go-seat player)
    player))

(defn game []
  (let [g (atom nil)]
    (engine/setup-web-game g)
    (let [l (engine/game-loop g)]
      (swap! game-loops conj l))
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
  (let [player (new-player player-id send-fn)]
    (engine/connect-player game player)))

(defn disconnect! [game player-id]
  (let [player (engine/find-player-with-id game player-id)]
      (close! (seat/out player))
      (close! (seat/in player))
      (engine/disconnect-player game player-id)))

(defn forward [player [topic data]]
  (put! (seat/out player) data))

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
        :rook/bid (forward player event)
        :rook/card (forward player event)
        :rook/kitty (forward player event)
        :rook/trump (forward player event)
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
    (doseq [game @game-loops]
      (close! game))
    (reset! game-loops [])
    (when stop-fn (stop-fn))
    (-> router
        (dissoc :stop-fn)
        (dissoc :handler))))

(defn new-router []
  (->Router nil nil nil))
