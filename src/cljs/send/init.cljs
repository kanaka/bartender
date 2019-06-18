(ns send.init
  (:require [cognitect.transit :as transit]
            [reagent.core :as r]
            [send.core :as core]
            [differ.core :as differ]
            [cljs.pprint :refer [pprint]]))

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


(defn connect [state uri]
  (let [ws (js/WebSocket. uri)
        treader (transit/reader :json)]
    (set! (.-onopen ws)
          (fn []
            (println "WebSocket connection opened")
            (swap! state assoc :connected true)))
    (set! (.-onclose ws)
          (fn []
            (println "WebSocket connection closed")
            (swap! state assoc :connected false)))
    (set! (.-onmessage ws)
          (fn [event]
            (let [msg (transit/read treader (.-data event))]
              (println "WebSocket message"
                       (:msgType msg)
                       "with" (count (.-data event)) "bytes")
              (msg-handler state msg))))))

(defn ^:export run []
  (connect core/state (str "ws://" js/location.host "/ws"))
  (r/render [core/main-element]
            (js/document.getElementById "app")))

(defn ^:export print-state []
  (pprint (assoc @core/state :test-state :ELIDED)))

(enable-console-print!)
(run)

