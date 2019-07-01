(ns mend.gen
  (:require [clojure.walk :refer [postwalk]]
            [instacheck.grammar :as grammar]))

;; Find each path in a grammar and replace it with :value
;; This allows us to replace stub generators in the grammar with
;; references to generators in a different namespace, avoid mutual
;; recursion, etc.
(def common-grammar-updates
  ;; Replace regex number generators with actual numeric/sized types
  {[:integer] {:tag :nt :keyword :gen/int}
   [:non-negative-integer] {:tag :nt :keyword :gen/nat}
   [:positive-integer] {:tag :nt :keyword :gen/s-pos-int}
   [:floating-point-number] {:tag :nt :keyword :gen/double}})

(def html5-grammar-updates
  {;; Replace the stub CSS value generator with real one
   [:attr-val-global__style] {:tag :nt :keyword :css-assignments-test}
   ;; Simplify rules that result in lots of noise/unicode
   [:name] {:tag :nt :keyword :rgen/simple-identifier}
   [:reference] {:tag :string :string "&#x00c9;"}
   [:comment] {:tag :string :string "<!-- HTML comment -->"}
   [:attribute-data] {:tag :nt :keyword :rgen/simple-identifier}
   [:aria-attribute] {:tag :epsilon}
   [:role-attribute] {:tag :epsilon}
   [:event-attribute] {:tag :epsilon}
   [:custom-data-attribute] {:tag :epsilon}
   ;; More with more efficient native generators
   [:attr-val-img__src] {:tag :nt :keyword :rgen/image-path}})

(def css3-grammar-updates
  {;; Remove recursive definitions
   [:nonprop-image] {:tag :nt :keyword :nonprop-url}
   [:nonprop-calc-value] {:tag :nt :keyword :any-number}
   ;; More with more efficient native generators
   [:nonprop-custom-ident] {:tag :cat
                            :parsers [{:tag :nt :keyword :rgen/simple-identifier}
                                      {:tag :string :string " "}]}
   ;; Remove css-unknown from css-declaration
   [:css-declaration :alt 0 :cat 0] {:tag :nt, :keyword :css-known-standard}
   ;; Simplify rules that result in lots of noise/unicode
   [:IDENT] {:tag :nt :keyword :gen/symbol}
   [:css-comment] {:tag :string :string "/* CSS comment */ "}
   [:quoted-string] {:tag :string :string "'QUOTED STRING'"}})

(defn replace-spaces
  "Replaces spaces with literal space strings."
  [x]
  (if (and (= :nt (:tag x))
           (#{:S :rS} (:keyword x)))
    {:tag :string :string (str " ")}
    x))

(defn reduce-strings*
  [a p2]
  (let [p1 (last a)]
    (if (and (= :string (:tag p1)) (= :string (:tag p2)))
      ;; Two strings, concatenate
      (conj (pop a) {:tag :string :string (str (:string p1) (:string p2))})
      (conj a p2))))

(defn reduce-strings
  "Combine strings and spaces that immediately follow each other in
  a concatenation."
  [x]
  (if (and (= :cat (:tag x))
           (:parsers x))
    (assoc x :parsers (reduce reduce-strings* [] (:parsers x)))
    x))

(defn grammar-update
  [grammar mode]
  (as-> grammar g
    (grammar/apply-grammar-update g common-grammar-updates)
    (grammar/apply-grammar-update g (condp = mode
                                      :html html5-grammar-updates
                                      :css css3-grammar-updates))
    (postwalk replace-spaces g)
    (postwalk reduce-strings g)))
