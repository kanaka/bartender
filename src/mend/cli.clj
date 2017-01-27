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

(declare single-pipe adjacent components double-amp double-pipe
         component component-single component-multiplied brackets
         braces)

(defn single-pipe
  "One of the values must occur."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (adjacent (drop 1 (first tree)) indent)
      (str pre "(gen/frequency [\n"
           (clojure.string/join
             "\n"
             (for [t tree]
               (str pre "  [100\n" (adjacent (drop 1 t) (+ 2 indent)) "]")))
           "])"))))

(defn adjacent
  "Each value must occur."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (components (second (first tree)) indent)
      (str pre "(gen/tuple\n"
           (clojure.string/join
             "\n"
             (for [t tree]
               (if (= :comma (first t))
                 (str pre "  (gen/return \",\")")
                 (components (second t) (+ 1 indent)))))
           ")"))))

(defn components [tree indent]
  (condp = (first tree)
    :double-amp  (double-amp  (drop 1 tree) indent)
    :double-pipe (double-pipe (drop 1 tree) indent)))

(defn double-amp
  "All values must occur in any order."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (component (second (first tree)) indent)
      (str pre "(gen/let [lst (gen/tuple \n"
           (clojure.string/join
             "\n"
             (for [t tree]
               (component (second t) (+ 8 indent))))
           ")]\n"
           pre "  (shuffle lst))"))))

(defn double-pipe
  "One or more of the values must occur in any order."
  [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (component (second (first tree)) indent)
      (str pre "(gen/let [cnt (gen/choose 1 " (count tree) ")\n"
           pre "          lst (gen/tuple \n"
           (clojure.string/join
             "\n"
             (for [t tree]
               (component (second t) (+ 8 indent))))
           ")]\n"
           pre "  (take cnt (shuffle lst)))"))))

(defn component [tree indent]
  (condp = (first tree)
    :component-single     (component-single (second tree) indent)
    :component-multiplied (component-multiplied (drop 1 tree) indent)))

(defn component-single [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (condp = (first tree)
      :literal       (str pre "(gen/return \"" (second tree) "\")")
      :keyword-value (str pre "(gen/return \"" (second tree) "\")")
      :non-property  (str pre "gen-nonprop-" (second tree))
      :property      (str pre "gen-prop-" (second tree))
      :brackets      (brackets (drop 1 tree) indent))))

(defn component-multiplied [[tree multiplier] indent]
  (let [pre (apply str (repeat indent "  "))
        single (component-single (second tree) (+ 1 indent))]
    (condp = (first (second multiplier))
      :question    (str pre "(gen/one-of [(gen/return \"\")\n" single "])")
      :asterisk    (str pre "(gen/vector\n" single ")")
      :plus        (str pre "(gen/such-that not-empty (gen/vector\n" single "))")
      :hash        (str pre "(gen/fmap #(interpose \" , \" %) (gen/such-that not-empty (gen/vector\n" single ")))")
      :braces      (braces (second (second multiplier)) single indent)
      :hash-braces (str pre "(gen/fmap #(interpose \" , \" %)\n"
                        (braces (second (second (second multiplier)))
                                (component-single (second tree) (+ 2 indent))
                                (+ 1 indent))")"))))

(defn brackets [tree indent]
  ;; TODO: deal with bang?
  (single-pipe (drop 1 (first tree)) indent))

(defn braces [kind single indent]
  (prn :here1 kind single)
  (let [pre (apply str (repeat indent "  "))
        bmin (apply str (drop 1 (second kind)))
        bmax (apply str (drop 1 (nth kind 2 nil)))]
    (prn :here2 bmin bmax)
    (condp = (first kind)
      :bracesA   (str pre "(gen/vector \n"
                      single "\n"
                      pre "  " bmin ")")
      :bracesA-B (str pre "(gen/vector \n"
                      single "\n"
                      pre "  " bmin " " bmax ")")
      ;; TODO: fix this 20 value
      :bracesA-  (str pre "(gen/vector \n"
                      single "\n"
                      pre "  " bmin " 20)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generator-name [k]
  (if (= \' (first k))
    (str "gen-prop-" (clojure.string/replace k #"'" ""))
    (str "gen-nonprop-" k)))

(defn prefix [ns]
  (str
"(ns " ns "
   (:require [clojure.test.check.generators :as gen]))\n"
"
(def gen-nonprop-integer gen/int)

(def gen-nonprop-length gen/pos-int)

(def gen-nonprop-color (gen/return \"red\"))

(defn flatten-text* [tree]
  (lazy-seq
    (cond
      (= \"\" tree)
      (list)

      (or (number? tree) (string? tree))
      (list tree \" \")

      :else
      (mapcat flatten-text* tree))))

(defn flatten-text [tree]
  (clojure.string/trimr
    (clojure.string/replace
      (apply str (flatten-text* tree))
      #\" +\" \" \")))

"))

(defn map->generators [m]
  (apply str
         (for [[k v] m]
           (str "(def " (generator-name k) "\n"
                "  (gen/fmap flatten-text\n"
                (single-pipe (drop 1 v) 2) "))\n\n"))))

(defn map->ns [ns m]
  (str (prefix ns) (map->generators m)))
