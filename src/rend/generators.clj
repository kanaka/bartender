(ns rend.generators
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hiccup.core :as hiccup]))

(def simple-name
  (gen/fmap #(apply str %1)
            (gen/tuple gen/char-alpha gen/string-alphanumeric)))

(def simple-keyword
  (gen/fmap #(keyword %1) simple-name))

(def image-path
  (gen/frequency
    [[10 (gen/return "/gen/static/Books-aj.svg_aj_ashton_01g_32x32.png")]
     [5  (gen/return "/gen/static/Books-aj.svg_aj_ashton_01g.png")]
     [10 (gen/return "/gen/static/B_stop.svg")]
     [5  (gen/return "/gen/static/Eilat_-_Dolphin_reef_100x67.jpg")]
     [2  (gen/return "/gen/static/Eilat_-_Dolphin_reef_300x200.jpg")]
     [1  (gen/return "/gen/static/Eilat_-_Dolphin_reef_600x400.jpg")]
     [10 (gen/return "/gen/static/SpaceX_Kestrel_engine2_25x24.gif")]
     [3  (gen/return "/gen/static/SpaceX_Kestrel_engine2.gif")]]))

(def img
  (gen/let [src image-path]
    (gen/return [:img {:src src}])))

(def tag-name
  (gen/frequency
    [[100 (gen/return :div)]
     [30  (gen/return :span)]
     [20  (gen/return :pre)]
     [10  (gen/return :main)]
     [10  (gen/return :header)]
     [10  (gen/return :footer)]
     [10  simple-name]           ;; random named tag
     [5   (gen/return :h1)]
     [4   (gen/return :h2)]
     [3   (gen/return :h3)]
     [2   (gen/return :h4)]
     [1   (gen/return :h5)]
     ]))

(def attr-name
  (gen/frequency
    [[30 (gen/return :class)]
     [30 (gen/return :id)]
     [30 simple-keyword]         ;; random attribute name
     ]))

(def attr-value simple-name)

(defn tag
  [inner]
  (gen/frequency
    [[20  img]
     [100 (gen/let [t tag-name
                    a (gen/map attr-name attr-value)]
            (gen/fmap #(apply vector t a %) (gen/vector inner)))]]))

(def content
  (gen/recursive-gen tag gen/string-alphanumeric))

(def body
  (gen/tuple (gen/return :body) (gen/return {:style "background:#1289ef;"}) content))

(def html
  (gen/tuple (gen/return :html) body))

;;(hiccp/html (gen/generate html 30))
;;
;;(defn qc-try
;;  [seed]
;;  (let [p (prop/for-all [a gen/pos-int] (> (* a a) a))]
;;    (println "start qc")
;;    (tc/quick-check 100 p
;;                    :seed seed
;;                    :max-size 50
;;                    :report-fn (fn [m]
;;                                 (prn :report m)))
;;    (println "finish qc")))

(defn rend-check [opts check-fn report-fn]
  (let [{:keys [iterations seed max-size]
         :or {:iterations 10
              :seed 1
              :max-size 50}} opts
        p (prop/for-all* [html] check-fn)]
    (tc/quick-check iterations p
                    :seed seed
                    :max-size max-size
                    :reporter-fn report-fn)))

