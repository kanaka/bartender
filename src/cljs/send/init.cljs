(ns send.init
  (:require [reagent.core :as r]
            [cljs.pprint :refer [pprint]]
            [cemerick.url :refer [url]]

            [send.core :as core]
            [send.net :as net]
            [send.render :as render]))

(defn ^:export start []
  (let [query (into {} (for [[k v] (:query (url js/location.href))]
                         [(keyword k) v]))]
    ;; If testid is specified as a query parameter, then we load the
    ;; top-level EDN data for the testid via a GET request. Otherwise,
    ;; we connect via WebSockets to the server to get both the current
    ;; state and state deltas over time.
    (if (:testid query)
      (net/load-edn (str "/gen/" (:testid query) ".edn")
                    #(core/msg-handler core/state {:msgType :full :data %}))
      (net/ws-connect core/state
                      (str "ws://" js/location.host "/ws")
                      core/msg-handler))
    (r/render [render/main-element]
              (js/document.getElementById "app"))))

(enable-console-print!)
(start)

