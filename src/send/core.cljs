(ns send.core
  (:require [reagent.core :as r]
            [differ.core :as differ]
            [cljs.pprint :refer [pprint]]

            [send.net :as net]
            [rend.util :as util]))

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
      (do (prn :patch-data data)
      (swap! state update-in [:test-state] differ/patch data))

      :merge
      (swap! state (fn [{:keys [test-state] :as cur-state}]
                     (assoc cur-state :test-state
                            (util/merge-test-state cur-state data))))

      (println "ignoring msgType" msgType))
    ;; Sync tab state
    (swap! state update-tabs)))

(defn connect-or-load
  [& {:keys [files ws-url]}]
  ;; If files is specified as a query parameter, then we load the
  ;; top-level EDN data for the files via a GET requests. Otherwise,
  ;; we connect via WebSockets to the server to get both the current
  ;; state and state deltas over time.
  (prn :files files :ws-url ws-url)
  (cond
    files (doseq [file files]
            (net/load-edn file
                          #(msg-handler state {:msgType :merge :data %})))
    ws-url (net/ws-connect state ws-url msg-handler)
    :else (throw (ex-info "connect-or-load requires files or ws-url" {}))))

(defn ^:export print-log-state []
    (pprint (assoc-in @state [:test-state :log] :ELIDED)))

(defn ^:export print-top-state []
    (pprint (assoc-in @state [:test-state] :ELIDED)))


