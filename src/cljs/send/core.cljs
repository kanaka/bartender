(ns send.core
  (:require [reagent.core :as r]
            [differ.core :as differ]
            [cljs.pprint :refer [pprint]]

            [send.net :refer [load-edn]]))

(defonce state
  (r/atom {:connected false
           :tabs {}
           :test-state nil}))

(defn update-tabs [state-val]
  (let [cur-tabs (:tabs state-val)
        slugs (-> state-val :test-state :test-slugs)
        all-tabs (into {} (for [slug slugs]
                            [slug {:visible false
                                   :thumbs  false}]))]
    ;; Add new tabs without touching existing ones by merging
    ;; (shallow) the current tabs last
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

(defn ^:export print-state []
    (pprint (assoc-in @state [:test-state :log] :ELIDED)))

(defn ^:export print-top-state []
    (pprint (assoc-in @state [:test-state] :ELIDED)))


