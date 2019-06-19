(ns send.init
  (:require [reagent.core :as r]
            [differ.core :as differ]
            [cljs.pprint :refer [pprint]]
            [cemerick.url :refer [url]]

            [send.net :as net]
            [send.core :as core])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn update-tabs [state-val]
  (let [cur-tabs (:tabs state-val)
        slugs (-> state-val :test-state :test-slugs)
        all-tabs (into {} (for [slug slugs]
                            [slug {:visible false
                                   :thumbs  false}]))]
    ;; Add new tabs without touching existing ones by merging
    ;; (shallow) the current tabs last
    (prn :all-tabs all-tabs :cur-tabs cur-tabs)
    (assoc state-val :tabs
           (merge all-tabs cur-tabs))))

(defn msg-handler [state msg]
  (let [{:keys [msgType data]} msg]
    ;;(prn :msgType msgType :data data)
    (condp = msgType
      :full
      (swap! state assoc :test-state data)

      :patch
      (swap! state update-in [:test-state] differ/patch data)

      (println "ignoring msgType" msgType))
    ;; Sync tab state
    (swap! state update-tabs)))

(defn ^:export run []
  (let [query (into {} (for [[k v] (:query (url js/location.href))]
                         [(keyword k) v]))]
    ;; If testid is specified as a query parameter, then we load the
    ;; top-level EDN data for the testid via a GET request. Otherwise,
    ;; we connect via WebSockets to the server to get both the current
    ;; state and state deltas over time.
    (if (:testid query)
      (net/load-edn (str "/gen/" (:testid query) ".edn")
                    #(msg-handler core/state {:msgType :full :data %}))
      (net/ws-connect core/state
                      (str "ws://" js/location.host "/ws")
                      msg-handler))
    (r/render [core/main-element]
              (js/document.getElementById "app"))))

(defn ^:export print-state []
  (pprint (assoc-in @core/state [:test-state :log] :ELIDED)))

(defn ^:export print-top-state []
  (pprint (assoc-in @core/state [:test-state] :ELIDED)))

(enable-console-print!)
(run)

