(ns send.init
  (:require [clojure.string :as S]
            [reagent.core :as r]
            [cemerick.url :refer [url]]

            [send.core :as core]
            [send.net :as net]
            [send.render :as render]))

(defn generic-start [reagent-elem dom-elem]
  (prn :starting reagent-elem dom-elem)
  (let [url (url js/location.href)
        query (into {} (for [[k v] (:query url)]
                         [(keyword k) v]))
        gen-dir (if (re-seq #"/static/" js/location.pathname)
                  "/gen/"
                  "gen/")
        config (merge
                 query
                 {:url url
                  :gen-dir gen-dir
                  :search js/location.search})]
    (swap! core/state assoc :config config)
    (if (:files query)
      (net/connect-or-load :files (S/split (:files query) #","))
      (net/connect-or-load :ws-url (str "ws://" js/location.host "/ws")))
    (r/render [reagent-elem] dom-elem)))

(defn ^:export send-monitor-start []
  (generic-start
    render/monitor-element (js/document.getElementById "app")))

(defn ^:export send-intro-start []
  (r/render
    [render/intro-element] (js/document.getElementById "app")))

(enable-console-print!)
