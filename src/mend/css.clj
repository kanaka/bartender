(ns mend.css
  (:require [mend.util :as util]
            [clojure.test.check.generators :as gen]
            [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

;; https://developer.mozilla.org/en-US/docs/Web/CSS/Value_definition_syntax
;; https://www.smashingmagazine.com/2016/05/understanding-the-css-property-value-syntax/
;; https://www.w3.org/TR/CSS21/grammar.html

;; TODO: global properties (inherit, etc)
(def css3-syntax-parser (insta/parser (slurp "src/mend/css3-syntax.ebnf")))

(defn parsed-tree->items [tree]
  (assert (= :assignments (first tree))
          (str "Parse tree started with "
               (first tree) " rather than :assignments"))
  (assert (every? #(and (vector? %) (= :assignment (first %)))
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

(defn check-and-merge-maps [maps & [override-map]]
  (doseq [k (set (mapcat keys maps))]
    (when (and (not (contains? override-map k))
               (apply not= (remove nil? (map #(get % k) maps))))
      (throw (Exception.
               (str "Multiple values found for key " k ":\n"
                    #_(vec (map #(get % k) maps)))))))
  (apply merge (concat maps override-map)))



(comment
  (def bs (css3-syntax-parser (slurp "./css-syntax/box-shadow.pvs")))
  (def tx (css3-syntax-parser (slurp "./css-syntax/transition.pvs")))
  (merge-maps (map parsed-tree->map [bs tx]))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generator-name [k]
  (cond
    (= \' (first k))
    (str "prop-" (string/replace k #"'" ""))

    (re-find #"\(\)$" k)
    (str "func-" (string/replace k #"\(\)$" ""))

    :else
    (str "nonprop-" k)))


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
           (string/join
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
           (string/join
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
           (string/join
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
           (string/join
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
      :non-property  (str pre "gen-" (generator-name (second tree)))
      :property      (str pre "gen-" (generator-name (second tree)))
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
  (let [pre (apply str (repeat indent "  "))
        bmin (apply str (drop 1 (second kind)))
        bmax (apply str (drop 1 (nth kind 2 nil)))]
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


;; Some base generators/types that are assumed

(def gen-nonprop-integer gen/int)

(def gen-nonprop-positive-integer gen/pos-int)

(def gen-nonprop-number (gen/frequency [[100 gen/int]
                                        [100 gen/double]]))

;; https://developer.mozilla.org/en-US/docs/Web/CSS/length
(def gen-length-unit (gen/frequency [;; font-relative
                                     [100 (gen/return \"em\")]
                                     [100 (gen/return \"ex\")]
                                     [100 (gen/return \"ch\")]
                                     [100 (gen/return \"rem\")]
                                     ;; viewport-percentage units
                                     [100 (gen/return \"vh\")]
                                     [100 (gen/return \"vw\")]
                                     [100 (gen/return \"vmin\")]
                                     [100 (gen/return \"vmax\")]
                                     ;; absolute length units
                                     [100 (gen/return \"px\")]
                                     [100 (gen/return \"mm\")]
                                     [100 (gen/return \"q\")]
                                     [100 (gen/return \"cm\")]
                                     [100 (gen/return \"in\")]
                                     [100 (gen/return \"pt\")]
                                     [100 (gen/return \"pc\")]]))

(def gen-nonprop-length (gen/fmap (fn [[i l]] (str i l))
                                  (gen/tuple gen/pos-int gen-length-unit)))

(def gen-nonprop-hex-color3 (gen/fmap #(format \"#%03x\" %) (gen/choose 0 0xfff)))
(def gen-nonprop-hex-color6 (gen/fmap #(format \"#%06x\" %) (gen/choose 0 0xffffff)))

(def gen-nonprop-hex-color (gen/frequency [[100 gen-nonprop-hex-color3]
                                           [100 gen-nonprop-hex-color6]]))

(def gen-nonprop-percentage (gen/fmap #(str % \"%\") (gen/choose 0 100)))

(def gen-nonprop-id-selector
  (->> (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric))
       (gen/fmap (fn [[c cs]] (apply str (cons c cs))))))

;; TODO:
(def gen-nonprop-url (gen/return \"http://blah.com/path/to/somehting\"))
(def gen-nonprop-string
  (->> (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric))
       (gen/fmap (fn [[c cs]] (apply str (cons c cs))))))
(def gen-nonprop-custom-ident (gen/return \"gen_custom_ident\"))
(def gen-nonprop-angle (gen/return \"90\"))
(def gen-nonprop-padding-left (gen/return \"10\"))
(def gen-nonprop-width (gen/return \"10\"))
(def gen-nonprop-max-width (gen/return \"10\"))
(def gen-nonprop-border-image-source (gen/return \"border_image_source\"))
(def gen-nonprop-border-image-slice (gen/return \"border_image_slice\"))
(def gen-nonprop-border-image-width (gen/return \"20\"))
(def gen-nonprop-border-image-outset (gen/return \"30\"))
(def gen-nonprop-border-image-repeat (gen/return \"40\"))
(def gen-nonprop-flex (gen/return \"gen_flex\"))
(def gen-nonprop-flex-direction (gen/return \"gen_flex_direction\"))
(def gen-nonprop-flex-wrap (gen/return \"gen_flex_wrap\"))
(def gen-nonprop-text-emphasis-style (gen/return \"gen_text_emphasis_style\"))
(def gen-nonprop-text-emphasis-color (gen/return \"gen_text_emphasis_color\"))
(def gen-nonprop-left (gen/return \"gen_left\"))
(def gen-nonprop-border-width (gen/return \"20\"))
(def gen-nonprop-border-style (gen/return \"gen_border_style\"))
(def gen-nonprop-outline-color (gen/return \"gen_outline_color\"))
(def gen-nonprop-outline-style (gen/return \"gen_outline_style\"))
(def gen-nonprop-outline-width (gen/return \"50\"))
(def gen-nonprop-x (gen/return \"11\"))
(def gen-nonprop-y (gen/return \"11\"))
(def gen-nonprop-time (gen/return \"gen_time\"))
(def gen-nonprop-grid-template (gen/return \"gen_grid_template\"))
(def gen-nonprop-grid-template-rows (gen/return \"gen_grid_template_rows\"))
(def gen-nonprop-grid-template-columns (gen/return \"gen_grid_template_columns\"))
(def gen-nonprop-grid-auto-rows (gen/return \"gen_grid_auto_rows\"))
(def gen-nonprop-grid-auto-columns (gen/return \"gen_grid_auto_columns\"))
(def gen-nonprop-grid-row-gap (gen/return \"gen_grid_row_gap\"))
(def gen-nonprop-grid-column-gap (gen/return \"gen_grid_column_gap\"))
(def gen-nonprop-column-width (gen/return \"gen_column_width\"))
(def gen-nonprop-column-count (gen/return \"gen_column_count\"))
(def gen-nonprop-column-rule-width (gen/return \"gen_column_rule_width\"))
(def gen-nonprop-column-rule-style (gen/return \"gen_column_rule_style\"))
(def gen-nonprop-column-rule-color (gen/return \"gen_column_rule_color\"))
(def gen-nonprop-font-style (gen/return \"gen_font_style\"))
(def gen-nonprop-font-weight (gen/return \"gen_font_weight\"))
(def gen-nonprop-font-stretch (gen/return \"gen_font_stretch\"))
(def gen-nonprop-font-size (gen/return \"gen_font_size\"))
(def gen-nonprop-line-height (gen/return \"gen_line_height\"))
(def gen-nonprop-font-family (gen/return \"gen_font_family\"))
(def gen-nonprop-list-line-style (gen/return \"gen_list_line_style\"))
(def gen-nonprop-list-style-type (gen/return \"gen_list_style_type\"))
(def gen-nonprop-list-style-position (gen/return \"gen_list_style_position\"))
(def gen-nonprop-list-style-image (gen/return \"gen_list_style_image\"))
(def gen-nonprop-resolution (gen/return \"gen_resolution\"))
(def gen-nonprop-margin-left (gen/return \"gen_margin_left\"))
(def gen-nonprop-min-width (gen/return \"gen_min_width\"))
(def gen-nonprop-text-decoration-line (gen/return \"gen_text-decoration-line\"))
(def gen-nonprop-text-decoration-style (gen/return \"gen_text-decoration-style\"))
(def gen-nonprop-text-decoration-color (gen/return \"gen_text-decoration-color\"))
(def gen-nonprop-background-color (gen/return \"gen_background-color\"))


;; Generated generators

"))

(defn value-generator-def [k v]
  (str "(def gen-" (generator-name k) "\n"
       "  (gen/fmap flatten-text\n"
       (single-pipe (drop 1 v) 2) "))"))

(defn assignment-generator [m]
  (str "(def gen-assignment\n"
       "  (gen/frequency [\n"
       (string/join
         "\n"
         (for [[k v] m
               :when (= \' (first k))
               :let [p (string/replace k #"'" "")]]
           (str "    [100 (gen/fmap #(str \"" p ": \" %)"
                " gen-" (generator-name k) ")]")))
       "\n    ]))"))

(defn map->ns [ns m]
  (let [prop-deps (util/tree-deps m)
        prop-order (util/topology-sort prop-deps)
        gen-strs (for [p prop-order]
                   (value-generator-def p (get m p)))
        ]
    (str (prefix ns)
         (string/join "\n\n" gen-strs)
         "\n\n"
         (assignment-generator m)
         "\n\n"
         "(def gen-assignments\n"
         "  (gen/fmap #(clojure.string/join \"; \" %)\n"
         "            (gen/vector gen-assignment)))\n"
         "\n\n"
         )))

(comment
  (def bs-pvs (slurp "css-syntax/box-shadow.pvs"))
  (def bs-tree (css3-syntax-parser bs-pvs))
  (def bs-map (parsed-tree->map bs-tree))
  (def bs-ns (map->ns "mend.box-shadow-generators" bs-map))
  (spit "src/mend/box_shadow_generators.clj" bs-ns)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def special ["unset.pvs" "inherit.pvs" "revert.pvs" "initial.pvs"])

(def broken ["filter.pvs"           ;; three formal syntax sections
             "vertical-align.pvs"   ;; no formal syntax section
             "image-resolution.pvs" ;; undefined/404
             "ruby-merge.pvs"       ;; undefined/404
             "font-variant-alternates.pvs" ;; legit failure to parse
             "font-variant.pvs"     ;; legit failure to parse
             "clip.pvs"             ;; legit failure to parse
             ])

(def OVERRIDES-FILE "_OVERRIDES_.pvs")

(defn load-pvs-files
  [dir & [skip]]
  (let [d (io/file dir)
        all-files (file-seq d)
        pvs-files (filter #(re-matches #".*\.pvs$" (.getName %)) all-files)
        ;; Skip the overrides files
        skip-set (conj (set skip) OVERRIDES-FILE)
        files (vec (filter #(not (skip-set (.getName %))) pvs-files))
        _ (println (str "Loading " (count files)
                        " syntax trees (skipping "
                        (- (count pvs-files) (count files)) ")"))
        trees (map #(css3-syntax-parser (slurp %)) files)
        fails (keep-indexed #(when (insta/failure? %2)
                                      [(.getName (get files %1)) %2])
                                   trees)]
    ;(prn :fails fails)
    (assert (not (seq fails))
            (str "Failed to parse files: " (vec (map first fails))
                 "\n\nErrors:\n" (vec fails)))
    trees))

(comment
  (def override-tree (css3-syntax-parser (slurp "css-syntax/_OVERRIDES_.pvs")))
  (def override-map (parsed-tree->map override-tree))

  (def css-trees (load-pvs-files "css-syntax/" (concat special broken)))
  (def css-maps (map parsed-tree->map css-trees))
  (def css-map (check-and-merge-maps css-maps override-map))

  (def css-ns (map->ns "mend.css-generators" css-map))
  (spit "src/mend/css_generators.clj" css-ns)
  (require '[mend.css-generators :as css-gen] :reload)
  (pprint (gen/sample css-gen/gen-prop-box-shadow 10))
)
