(ns mend.html
  (:require [clojure.test.check.generators :as gen]
            [instaparse.core :as insta]))

;; https://developer.mozilla.org/en-US/docs/Web/HTML/Reference
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
;; - XML ebnf:
;;   http://bottlecaps.de/rr/ui
;;   https://www.w3.org/TR/2000/REC-xml-20001006
;; - HTML ebnf:
;;   https://github.com/lisp/de.setf.xml/blob/master/bnf/html-grammar.bnf

(def html5-parser (insta/parser (slurp "src/mend/html5.ebnf")))

(def html5-grammar (.grammar html5-parser))

(defn get-html-tags []
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:char-ref-hex :char-ref-dec :attribute :space :char-data :element :eq :content :name :name-unclosed :end-tag :reference :empty-tag :opt-space :entity-ref :attribute-value :comment :start-tag

(declare cat alt regexp element empty-tag start-tag content end-tag
         attribute attribute-value char-data reference entity-ref
         char-ref-dec char-ref-hex name- name-unclosed comment-)

(defn cat
  "Each value must occur."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    ))

(defn alt
  "One of the values must occur."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/one-of [\n"
         pre "  " 123 "])")))

(defn regexp
  "A regexp"
  []
  )

(defn element
  ""
  [])

(defn content
  ""
  [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prefix [ns]
  (str
"(ns " ns "
   (:require [clojure.test.check.generators :as gen]))\n"
"
(defn flatten-text* [tree]
  (lazy-seq
    (cond
      (= \"\" tree)                       (list)
      (or (number? tree) (string? tree))  (list tree \" \")
      :else                               (mapcat flatten-text* tree))))

(defn flatten-text [tree]
  (clojure.string/trimr
    (clojure.string/replace
      (apply str (flatten-text* tree))
      #\" +\" \" \")))


;; Same base generators/types that are assumed

(def gen-nonprop-integer gen/int)

(def gen-nonprop-length gen/pos-int)

"))


(defn map->generators [m]
  (apply
    str
    (for [[k v] m]
      (cond
        (= :cat (:tag v))   (cat v indent)
        (= :alt (:tag v))   (alt v indent)
        (= :regex (:tag v)) (regex v indent)
        (= :element k)      (element v indent)
        (= :content k)      (content v indent)))))

(defn map->ns [ns m]
  (str (prefix ns) (map->generators m)))
