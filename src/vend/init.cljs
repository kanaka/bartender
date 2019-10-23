(ns vend.init
  (:require [send.init]
            [vend.render :as vrender]))

(defn ^:export tapv-tables-start []
  (send.init/generic-start
    vrender/tapv-tables-element (js/document.getElementById "app")))

(defn ^:export flat-table-start []
  (send.init/generic-start
    vrender/flat-table-element (js/document.getElementById "app")))
