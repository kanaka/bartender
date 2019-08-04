(ns mend.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [hickory.core]
            [hickory.render]
            [hickory.select :as s]

            [instacheck.core :as instacheck]))

(def EBNFs
  {:html {:gen          ["html5-test.ebnf" "html5.ebnf"]
          :parse        ["html5.ebnf"]}
   :css  {:gen          ["css3-test.ebnf" "css3.ebnf"]
          :parse        ["css3.ebnf"]
          :parse-inline ["css3.ebnf"]}
   :tNa  {:parse        ["tags-and-attrs.ebnf"]}})

(def GRAMMARs
  {:html "html5.grammar"
   :css  "css3.grammar"})


(def GRAMMAR-MANGLES
  {:html {:gen          {:char-data :char-data-test
                         :comment   :comment-test
                         :url       :url-test}
          :parse        {}}
   :css  {:gen          {}
          :parse        {:nonprop-group-rule-body  :stylesheet
                         :prop-group-rule-body     :css-ruleset
                         :nonprop-declaration-list :css-assignments}
          :parse-inline {:nonprop-group-rule-body  :stylesheet
                         :prop-group-rule-body     :css-ruleset
                         :nonprop-declaration-list :css-assignments}}})

(def START-RULES
  {:html {:gen           :html-test
          :parse         :html}
   :css  {:gen           :css-assignments-test
          :parse         :stylesheet
          :parse-inline  :css-assignments}
   :tNa  {:parse         :html}})

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

(defn load-parser [kind mode]
  (let [parser (load-parser* (get-in EBNFs [kind mode])
                             (get-in GRAMMAR-MANGLES [kind mode]))
        start (get-in START-RULES [kind mode])]
    (assoc parser :start-production start)))

(defn load-parser-from-grammar [kind mode]
  (let [gfile (io/resource (get GRAMMARs kind))
        grammar (read-string (slurp gfile))
        start (get-in START-RULES [kind mode])]
    (instacheck/grammar->parser grammar start)))
