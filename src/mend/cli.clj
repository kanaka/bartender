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
  (assert (every? #(and (vector? %) (= :property (first %)))
                  (drop 1 tree))
          "Parse tree contained invalid property")
  (for [prop (drop 1 tree)]
    (let [ptype (first (second prop))
          name (second (second prop))
          data (nth prop 2)]
      (condp = ptype
        :property-type
        [(str "'" name "'") data]
        :non-property-type
        [name data]))))

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
