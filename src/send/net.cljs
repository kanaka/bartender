(ns send.net
  (:require [clojure.edn :as edn]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [cognitect.transit :as transit]
            [send.core :as core])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn load-edn [path callback]
  (go
    (let [{:keys [body status]} (<! (http/get path))
          edn (if (string? body)
                (edn/read-string body)
                body)]
      (prn :status status)
      (prn :body-count (count edn))
        (callback edn))))

(defn load-transit [path callback]
  (go
    (let [{:keys [body status]} (<! (http/get path))
          rdr (transit/reader :json)
          data (if (string? body)
                (transit/read rdr body)
                body)]
      (prn :status status)
      (prn :body-count (count body))
      (callback data))))


(defn ws-connect [state uri msg-handler]
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

(defn connect-or-load
  [& {:keys [files ws-url]}]
  ;; If files is specified as a query parameter, then we load the
  ;; top-level EDN data for the files via a GET requests. Otherwise,
  ;; we connect via WebSockets to the server to get both the current
  ;; state and state deltas over time.
  (prn :files files :ws-url ws-url)
  (cond
    files (doseq [file files]
            (if (re-seq #"\.transit$" file)
              (load-transit
                file #(core/msg-handler core/state {:msgType :merge :data %}))
              (load-edn
                file #(core/msg-handler core/state {:msgType :merge :data %}))))
    ws-url (ws-connect core/state ws-url core/msg-handler)
    :else (throw (ex-info "connect-or-load requires files or ws-url" {}))))

