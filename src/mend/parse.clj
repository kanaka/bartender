(ns mend.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [hickory.core]
            [hickory.render]
            [hickory.select :as s]

            [instacheck.core :as instacheck]))

(def EBNF-PATHS
  {:html {:gen   ["html5-test.ebnf" "html5.ebnf"]
          :parse ["html5.ebnf"]}
   :css  {:gen   ["css3-test.ebnf" "css3.ebnf"]
          :parse ["css3.ebnf"]}})

(def GRAMMAR-PATHS
  {:html "html5.grammar"
   :css  "css3.grammar"})


(def GRAMMAR-MANGLES
  {:html {:gen   {:char-data :char-data-test
                  :comment :comment-test
                  :url :url-test}
          :parse {}}
   :css  {:gen   {}
          :parse {:nonprop-group-rule-body :stylesheet
                  :prop-group-rule-body :css-ruleset
                  :nonprop-declaration-list :css-assignments}}})

(def START-RULES
  {:html {:gen    :html-test
          :parse  :html}
   :css  {:gen    :css-assignments-test
          :parse  :stylesheet}})

(defn mangle-parser
  [parser mangles]
  (reduce (fn [p [k v]] (assoc-in p [:grammar k]
                                  {:tag :nt, :keyword v}))
          parser mangles))

(defn load-parser* [paths mangles]
  (let [ebnf (string/join "\n" (map slurp (map io/resource paths)))
        base-parser (instacheck/load-parser ebnf)
        parser (mangle-parser base-parser mangles)]
    parser))

(defn load-parser [mode direction]
  (let [parser (load-parser* (get-in EBNF-PATHS [mode direction])
                             (get-in GRAMMAR-MANGLES [mode direction]))]
    (assoc parser :start-production (get-in START-RULES [mode direction]))))

(defn load-parser-from-grammar [mode]
  (let [gfile (io/resource (get GRAMMAR-PATHS mode))
        grammar (read-string (slurp gfile))
        start (get-in START-RULES [mode :parse])]
    (instacheck/grammar->parser grammar start)))
