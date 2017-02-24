(ns mend.ebnf
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [alandipert.kahn :as kahn]
            [clojure.java.io :refer [as-file]]
            [mend.util :as util]
            [clojure.walk :as walk]

            ;; Not actually used here, but convenient for testing
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-grammar
  [ebnf-text]
  (util/remove-key (.grammar (insta/parser ebnf-text)) :red))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create test.check generators for sub-trees of an
;; instaparse grammar rule

(declare gen-ROUTE)

(defn gen-cat
  "Each value must occur in order."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE ctx (-> tree :parsers first) indent)
      (str pre "(gen/tuple\n"
           (string/join
             "\n"
	     (for [t (-> tree :parsers)]
	       (gen-ROUTE ctx t (+ 1 indent))))
           ")"))))


(defn gen-alt
  "One of the values must occur."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE ctx (-> tree :parsers first) indent)
      (str pre "(gen/frequency [\n"
           (string/join
             "\n"
	     (for [t (-> tree :parsers)]
               (str pre "  [100\n" (gen-ROUTE ctx t (+ 2 indent)) "]")))
           "])"))))

(defn gen-regexp
  "Value must match regexp. For common space value \\s* and \\s+
  generate zero and 1 space respectively."
  [ctx tree indent]
  (let [re (:regexp tree)
        pre (apply str (repeat indent "  "))]
    (cond
      (= (str #"\s*") (str re))
      (str pre "(gen/return \"\")")

      (= (str #"\s+") (str re))
      (str pre "(gen/tuple (gen/return \" \"))")

      :else
      (str pre "(chuck/string-from-regex " (pr-str re) ")"))))

;; TODO: :hide option
(defn gen-string
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return " (pr-str (:string tree)) ")")))

(defn gen-star
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) ")")))

(defn gen-plus
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/such-that not-empty (gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "))")))

(defn gen-opt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/one-of [\n"
         pre "  (gen/return \"\")\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "])")))

(defn gen-epsilon
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return \"\")")))

;; TODO: mutual recursion?
;; TODO: :hide option
(defn gen-nt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))
        kw (:keyword tree)]
    (str pre (if (= ctx kw)
               "inner"
               (str "gen-" (name kw))))))

;;;;;;

(def tag-to-gen
  {:cat     gen-cat
   :alt     gen-alt
   :regexp  gen-regexp
   :string  gen-string
   :star    gen-star
   :plus    gen-plus
   :opt     gen-opt
   :epsilon gen-epsilon
   :nt      gen-nt})

(defn gen-ROUTE
  [ctx tree indent]
  (let [tag (:tag tree)
        f (get tag-to-gen (:tag tree))]
    (assert f (str "No generator found for " tag))
    (f ctx tree indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create generators for full instaparse grammar including
;; immediately recursive grammars.


(defn prune-rule-recursion
  "Prune a grammar rule of the recursive parts. Identify the smallest
  optional branches of the rule which are recursive and prune/remove
  them."
  [k tree]
  (let [parent? (fn [t] (util/tree-matches
                          #(= % {:tag :nt :keyword k}) t))]
    (walk/postwalk
      (fn [node]
        (if (and (map? node) (parent? node))
          ;; Prune/rewrite the matches sub-trees
          (condp = (:tag node)
            :alt  {:tag :alt
                   :parsers (filter #(not (parent? %))
                                    (:parsers node))}
            :star {:tag :epsilon}
            :opt  {:tag :epsilon}
            node)
          node))
      tree)))

(defn prune-grammar-recursion
  "The test.check gen-recursive generator takes a recursive generator
  and a non-recursive generator as options. For directly recursive
  grammar rules, a version of the rule without the recursion (smallest
  optional part of the rule that recurses is removed) is used for the
  non-recursive part of gen-recursive and the full recursive rule is
  used for the recursive generator to recursive-gen."
  [grammar]
  (into {} (for [[k rule] grammar]
             [k (prune-rule-recursion k rule)])))

;;;;;;

(defn gen-rule-body
  [k v indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (util/tree-matches #(= k %) v)
      (str pre "(gen/recursive-gen\n"
           pre "  (fn [inner]\n"
           (gen-ROUTE k v (+ 2 indent)) ")\n"
           (gen-ROUTE k (prune-rule-recursion k v) (+ 1 indent)) ")")
      (str (gen-ROUTE k v indent)))))

(defn gen-rule
  [k v indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(def gen-" (name k) "\n"
         pre "  (gen/fmap util/flatten-text\n"
         (gen-rule-body k v (+ 2 indent)) "))")))


(defn grammar->generators
  [grammar]
  (let [deps (util/tree-deps grammar)
        pruned-deps (kahn/remove-direct-recursive deps)
        _ (assert (empty? (kahn/mutually-recursive pruned-deps))
                  (str "Mutually recursive generators unsupported:"
                       (kahn/mutually-recursive pruned-deps)))
        ordered-nodes (reverse (kahn/kahn-sort-throws pruned-deps))]
    (string/join
      "\n\n"
      (for [k ordered-nodes]
        (gen-rule k (get grammar k) 0)))))

(comment

  (def ebnf-grammar (load-grammar (slurp "data/recur1.ebnf")))
  (grammar->generators ebnf-grammar)

)

;;;;;;

(defn prefix [ns]
  (str
"(ns " ns "
   (:require [clojure.test.check.generators :as gen]
             [com.gfredericks.test.chuck.generators :as chuck]
             [mend.util :as util]))

"))

(defn grammar->ns [ns grammar]
  (str (prefix ns) (grammar->generators grammar)))

