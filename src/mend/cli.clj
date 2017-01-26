(ns mend.cli
  (:require [org.httpkit.client :as http]
            [hickory.core :as hick]
            [hickory.select :as s]
            [clojure.walk :refer [walk]]
            [instaparse.core :as insta]
            [clojure.java.io :refer [as-file make-parents]]))

;; https://developer.mozilla.org/en-US/docs/Web/CSS/Reference
;;   - look at formal syntax section to generate properties
;; https://developer.mozilla.org/en-US/docs/Template:csssyntax
;;   - csssyntax macro that pulls syntax onto MDN page
;; https://github.com/mdn/data/tree/master/css
;;   - directory of CSS JSON data files (for csssyntax macro)
;; https://raw.githubusercontent.com/mdn/data/master/css/syntaxes.json
;;   - raw syntax data
;; http://www.blooberry.com/indexdot/css/propindex/all.htm
;; https://www.w3.org/Style/CSS/all-properties.en.html
;;
;; https://developer.mozilla.org/en-US/docs/Web/CSS/Value_definition_syntax
;; https://www.smashingmagazine.com/2016/05/understanding-the-css-property-value-syntax/
;; https://www.w3.org/TR/CSS21/grammar.html

(def CSS-BASE-URI "https://developer.mozilla.org")
(def CSS-REF-PATH "/en-US/docs/Web/CSS/Reference")

;; Takes a hickory block and extracts the text content adding
;; line-breaks where appropriate (<p> and <br>).
;; Returns a sequence of strings
;; This is not efficient (some non-TCO), but it works.
(defn inner-text [root]
  (loop [res []
         tree root]
    (cond
      (nil? (seq tree)) res
      (string? tree) [tree]
      (sequential? tree) (recur (concat res (inner-text (first tree)))
                                (next tree))
      (map? tree) (if (get #{:p :br} (:tag tree))
                    (recur (conj res "\n") (:content tree))
                    (recur res (:content tree)))
      :else nil)))

(defn get-css-keywords []
  (let [;; Get the MDN CSS reference page
        resp @(http/get (str CSS-BASE-URI CSS-REF-PATH))
        ;; Parse it
        all-data (hick/as-hickory (hick/parse (:body resp)))
        ;; Pull out the keyword index
        as (s/select
             (s/descendant
               (s/and (s/class "index")
                      s/first-child)
               (s/tag :ul)
               (s/tag :li)
               (s/tag :a))
             all-data)
        kw-map (into {}
                     (for [a as]
                       [(-> a :content first :content first)
                        (-> a :attrs :href)]))]
    kw-map))

;; Download the formal syntax for a single property
;; Returns a hickory format of the syntax HTML
(defn get-formal-syntax-hickory [path]
  (let [resp @(http/get (str CSS-BASE-URI path))
        all-data (hick/as-hickory (hick/parse (:body resp)))
        sb (s/select (s/class "syntaxbox")
                     all-data)]
    (:content (first sb))))

;; Takes the result of get-formal-syntax-hickory and outputs a full
;; formal syntax text string for the property. Strips the "where"
;; lines and prepends the property starting node name.
(defn formal-syntax [prop hickory-data]
  (let [pruned (filter #(not (re-find #"^ *where *$" %))
                       (inner-text hickory-data))]
    (str (apply str "<'" prop "'> = " pruned) "\n")))

(defn filter-css-properties [kws]
  (let [ffn (fn [[name path]]
              (and
                (re-find #"^[a-z-]+$" name)
                (not (re-find #"#" path))))]
    (into {} (filter ffn kws))))

(defn get-all-css-properties [outdir]
  (let [all-props (filter-css-properties (get-css-keywords))]
    (make-parents (str outdir "/file"))
    (doseq [[prop path] (sort all-props)]
      (let [full-path (str outdir "/" prop ".pvs")]
        (if (.exists (as-file full-path))
          (println "Already exists:" prop)
          (do
            (println "Retrieving:" prop)
            (let [pdata (get-formal-syntax-hickory path)
                  text (formal-syntax prop pdata)]
              (spit (str outdir "/" prop ".pvs") text))))))))

;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes

(defn get-html-tags []
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def css3-parser (insta/parser (slurp "./css3.ebnf")))

(defn parsed-tree->items [tree]
  (assert (= :syntax (first tree))
          "Parse tree did not start with :syntax")
  (assert (every? #(and (vector? %) (= :property-def (first %)))
                  (drop 1 tree))
          "Parse tree contained invalid property")
  (for [prop (drop 1 tree)]
    (let [ptype (first (second prop))
          name (second (second prop))
          data (nth prop 2)]
      (condp = ptype
        :property     [(str "'" name "'") data]
        :non-property [name data]))))

(defn parsed-tree->map [tree]
  (let [items (parsed-tree->items tree)]
    (assert (= (count items) (count (set (map first items))))
            "Repeated properties")
  (into {} items)))

(defn parsed-trees->map [trees]
  (apply
    merge-with
    (fn [left right]
      (if (= left right)
        left
        (throw (Exception. (str "Values conflict: " left " " right)))))
    (map parsed-tree->map trees)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def tx (parsed-tree->map (css3-parser (slurp "./css-syntax/transition.pvs"))))

(declare component component-multiplied component-single components
         double-amp single-pipe double-pipe adjacent)

(defn component [tree indent]
  (condp = (first tree)
    :component-single     (component-single (second tree) indent)
    :component-multiplied (component-multiplied (drop 1 tree) indent)
    :components           (components (second tree) indent)))

(defn component-single [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (condp = (first tree)
      :literal       (str pre "(gen/return \"" (second tree) "\")")
      :keyword-value (str pre "(gen/return \"" (second tree) "\")")
      :non-property  (str pre "(gen/return gen-nonprop-" (second tree) ")")
      :property      (str pre "(gen/return gen-prop-" (second tree) ")")
      :brackets      (component (second tree) indent))))

(defn component-multiplied [[tree multiplier] indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "GEN/multiplier")))

(defn components [tree indent]
  (condp = (first tree)
    :double-amp  (double-amp  (drop 1 tree) indent)
    :single-pipe (single-pipe (drop 1 tree) indent)
    :double-pipe (double-pipe (drop 1 tree) indent)
    :adjacent    (adjacent    (drop 1 tree) indent)))

(defn double-amp [tree indent]
  (str (apply str (repeat indent "  "))
       "GEN/double-amp"))

(defn single-pipe [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/frequency [\n"
         (clojure.string/join
           "\n"
           (for [t tree]
             (str pre "  [100\n" (component t (+ 2 indent)) "]")))
         "])")))

(defn double-pipe [tree indent]
  (str (apply str (repeat indent "  "))
       "GEN/double-pipe"))

(defn adjacent [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/tuple\n"
         (clojure.string/join
           "\n"
           (for [t tree]
             (component t (+ 1 indent))))
         ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generator-name [k]
  (if (= \' (first k))
    (str "gen-prop-" (clojure.string/replace k #"'" ""))
    (str "gen-nonprop-" k)))

(defn map->generators [m]
  (apply str
         (for [[k v] m]
           (str "(def " (generator-name k) "\n"
                (component v 1) ")\n\n"))))
