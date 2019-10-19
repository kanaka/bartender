(ns mend.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]

            [instacheck.core :as instacheck]))

(def EBNFs
  {:html {:gen          ["html5-test.ebnf" "html5.ebnf"]
          :parse        ["html5.ebnf"]}
   :css  {:gen          ["css3-test.ebnf" "css3.ebnf"]
          :parse        ["css3.ebnf"]
          :parse-inline ["css3.ebnf"]}})

(def GRAMMARs
  {:html "html5.grammar"
   :css  "css3.grammar"})


(def GRAMMAR-MANGLES
  {:html {:gen          {:char-data :char-data-test
                         :comment   :comment-test
                         :url       :url-test}
          :parse        {}}
   ;; Remove mutually recursive defintions
   :css  {:gen          {:nonprop-group-rule-body  "STUB_nonprop_group_rule_body"
                         :prop-group-rule-body     "STUB_prop_group_rule_body"
                         :nonprop-declaration-list "STUB_declaration_list"}
          :parse        {}
          :parse-inline {}}})

(def START-RULES
  {:html {:gen           :html-test
          :parse         :html}
   :css  {:gen           :css-assignments-test
          :parse         :stylesheet
          :parse-inline  :css-assignments}})

(defn mangle-parser
  [parser mangles]
  (reduce (fn [p [k v]] (assoc-in p [:grammar k]
                                  (cond
                                    (string? v) {:tag :string, :string v}
                                    (keyword? v) {:tag :nt, :keyword v})))
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
