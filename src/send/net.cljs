(ns send.net
  (:require [clojure.edn :as edn]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [cognitect.transit :as transit])
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

