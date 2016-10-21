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
  (gen/let [t tag-name
            a (gen/map attr-name attr-value)]
    (gen/fmap #(apply vector t a %) (gen/vector inner))))

(def content
  (gen/recursive-gen tag gen/string-alphanumeric))

(def body
  (gen/tuple (gen/return :body) content))

(def html
  (gen/tuple (gen/return :html) body))

;;(hiccps/html (gen/generate html 30))

(defn qc-try
  [seed]
  (let [p (prop/for-all [a gen/pos-int] (> (* a a) a))]
    (println "start qc")
    (tc/quick-check 100 p
                    :seed seed
                    :max-size 50
                    :report-fn (fn [m]
                                 (prn :report m)))
    (println "finish qc")))

