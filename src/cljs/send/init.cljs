(ns send.init
  (:require [clojure.string :as S]
            [reagent.core :as r]
            [cemerick.url :refer [url]]

            [send.core :as core]
            [send.render :as render]))

(defn generic-start [reagent-elem dom-elem]
  (prn :starting reagent-elem dom-elem)
  (let [query (into {} (for [[k v] (:query (url js/location.href))]
                         [(keyword k) v]))]
    (if (:files query)
      (core/connect-or-load :files (S/split (:files query) #","))
      (core/connect-or-load :ws-url (str "ws://" js/location.host "/ws")))
    (r/render [reagent-elem] dom-elem)))

(defn ^:export send-start []
  (generic-start
    render/main-element (js/document.getElementById "app")))

(enable-console-print!)
