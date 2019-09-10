(ns vend.init
  (:require [send.init]
            [vend.render :as vrender]))

(defn ^:export vend-start []
  (send.init/generic-start
    vrender/main-element (js/document.getElementById "app")))
