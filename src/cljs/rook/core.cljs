(ns rook.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! chan alts!]]
            [goog.dom :as gdom]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {}))

(defn app-view [state owner]
  (reify
    om/IRender
    (render [_]
      (dom/h1 nil "Hello"))))

(om/root app-view app-state
         {:target (gdom/getElement "board")})
