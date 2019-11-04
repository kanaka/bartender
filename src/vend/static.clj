(ns vend.static
  (:require [clojure.pprint :refer [pprint]]
            [hiccup.core :refer [html]]
            [send.core]
            [vend.render]
            [rend.util]))

(defn load-files
  [& files]
  (let [datums (for [file files]
                (if (re-seq #"\.transit$" file)
                  (assert false "implement transit support")
                  (read-string (slurp file))))]
    (apply rend.util/merge-test-state {} datums)))

(defn to-html
  [h]
  (html
    [:html
     [:head
      [:meta {:charset "utf-8"}]
      [:link {:href "tabbed.css"  :rel "stylesheet"}]
      [:link {:href "report.css"  :rel "stylesheet"}]
      [:link {:href "modal.css"   :rel "stylesheet"}]
      [:link {:href "tooltip.css" :rel "stylesheet"}]
      [:link {:href "vend.css"    :rel "stylesheet"}]]
     [:body
      h]]))

(defn render
  [mode config & [files]]
  (let [test-state (load-files files)
        _ (reset! send.core/state {:test-state test-state
                                   :config config})
        r (get {:tapv vend.render/tapv-tables-element
                 :flat vend.render/flat-table-element}
                mode)
        h (r)]
    (to-html h)))
