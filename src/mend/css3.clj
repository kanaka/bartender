(ns mend.css3
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]

            [alandipert.kahn :as kahn]
            [instaparse.core :as insta]
            [mend.util :as util]
            [clojure.tools.cli :refer [parse-opts]]

            ;; Not actually used here, but convenient for testing
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]))

;; TODO: global properties (inherit, initial, unset, revert)

;; https://developer.mozilla.org/en-US/docs/Web/CSS/Value_definition_syntax
;; https://www.smashingmagazine.com/2016/05/understanding-the-css-property-value-syntax/
;; https://www.w3.org/TR/CSS21/grammar.html
;; https://github.com/mdn/data/tree/master/css
;; https://github.com/csstree/csstree/
;; https://csstree.github.io/docs/syntax.html

(def css3-properties (json/read-str (slurp "mdn_data/css/properties.json")))
(def css3-syntaxes (json/read-str (slurp "mdn_data/css/syntaxes.json")))

(def css3-syntax-parser (insta/parser (slurp "data/css-pvs.ebnf")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn filter-css-properties
  "Filter properties we want (mostly just removes '--*')"
  [props]
  (into {} (filter (fn [[name val]]
                     (re-find #"^[a-z-]+$" name))
                   props)))

(defn mangle-css-syntaxes
  "Fix some bugs in the syntax definitions."
  [syntaxes]
  (into {} (for [[k v] syntaxes]
             (cond
               ;; TODO: file bug against github.com/mdn/data
               (= v "<custom-ident>: <integer>+;")
               [k "<custom-ident> : <integer>+ ;"]

               ;; TODO: file bug against github.com/mdn/data
               (= v "rect(<top>, <right>, <bottom>, <left>)")
               [k "rect( <top>, <right>, <bottom>, <left> )"]

               ;; Remove recursive part
               ;; TODO: is this really intended (find W3C standard)
               (= k "image")
               [k "<url> | <element()>"]

               ;; Drop unused syntaxes that also have recursion
               (= k "page-body")
               nil
               (.startsWith k "media-")
               nil
               (.startsWith k "calc")
               nil

               :else
               [k v]))))

(defn pvs [properties syntaxes]
  (let [props (filter-css-properties properties)
        syns (mangle-css-syntaxes syntaxes)
        ps (for [[prop {:strs [syntax]}] (sort props)]
             (str "<'" prop "'> = " syntax "\n"))
        ss (for [[syn syntax] (sort syns)]
             (str "<" syn "> = " syntax "\n"))]
    (apply str (concat ps ss))))

(comment
  (spit "data/css3.pvs" (pvs css3-properties css3-syntaxes))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; TODO: remove
(defn check-and-merge-maps [maps & [override-map]]
  (doseq [k (set (mapcat keys maps))]
    (when (and (not (contains? override-map k))
               (apply not= (remove nil? (map #(get % k) maps))))
      (throw (Exception.
               (str "Multiple values found for key " k ":\n"
                    #_(vec (map #(get % k) maps)))))))
  (apply merge (concat maps override-map)))



(comment
  ;; Takes 4 seconds
  (def parse-tree (css3-syntax-parser (slurp "data/css3.pvs")))
  (def parse-map (parsed-tree->map parse-tree))
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
         component component-single component-multiplied
         brackets block braces)

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
      :brackets      (brackets (drop 1 tree) indent)
      :block         (block    (drop 1 tree) indent))))

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

(defn block [tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/tuple\n"
         pre "  (gen/return \"{\")\n"
         (adjacent (drop 1 (second tree)) (+ 1 indent)) "\n"
         pre "  (gen/return \"}\"))")))

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

;; TODO: split this out into separate file
(defn prefix [ns]
  (str
"(ns " ns "
   (:require [mend.util :as util]
             [clojure.test.check.generators :as gen]
             [rend.misc-generators :as misc-gen]))

;; Generated by mend.css3

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

(def gen-nonprop-dimension (gen/return \"TODO_dimension\"))
(def gen-nonprop-ratio (gen/return \"TODO_ratio\"))

(def gen-nonprop-hex-color3 misc-gen/hex-color3)
(def gen-nonprop-hex-color6 misc-gen/hex-color6)

(def gen-nonprop-hex-color (gen/frequency [[100 gen-nonprop-hex-color3]
                                           [100 gen-nonprop-hex-color6]]))

(def gen-nonprop-percentage misc-gen/percentage)

(def gen-nonprop-id-selector
  (->> (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric))
       (gen/fmap (fn [[c cs]] (apply str (cons c cs))))))

;; TODO:
(def gen-nonprop-url (gen/return \"http://blah.com/path/to/somehting\"))
(def gen-nonprop-string
  (->> (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric))
       (gen/fmap (fn [[c cs]] (apply str (cons c cs))))))
(def gen-nonprop-custom-ident (gen/return \"TODO_custom_ident\"))
(def gen-nonprop-custom-property-name (gen/return \"TODO_custom_property\"))
(def gen-nonprop-ident (gen/return \"TODO_ident\"))
(def gen-nonprop-angle (gen/return \"90\"))
(def gen-nonprop-padding-left (gen/return \"10\"))
(def gen-nonprop-width (gen/return \"10\"))
(def gen-nonprop-max-width (gen/return \"10\"))
(def gen-nonprop-border-radius (gen/return \"TODO_border_radius\"))
(def gen-nonprop-border-image-source (gen/return \"border_image_source\"))
(def gen-nonprop-border-image-slice (gen/return \"border_image_slice\"))
(def gen-nonprop-border-image-width (gen/return \"20\"))
(def gen-nonprop-border-image-outset (gen/return \"30\"))
(def gen-nonprop-border-image-repeat (gen/return \"40\"))
(def gen-nonprop-flex (gen/return \"TODO_flex\"))
(def gen-nonprop-flex-direction (gen/return \"TODO_flex_direction\"))
(def gen-nonprop-flex-wrap (gen/return \"TODO_flex_wrap\"))
(def gen-nonprop-text-emphasis-style (gen/return \"TODO_text_emphasis_style\"))
(def gen-nonprop-text-emphasis-color (gen/return \"TODO_text_emphasis_color\"))
(def gen-nonprop-left (gen/return \"TODO_left\"))
(def gen-nonprop-top (gen/return \"TODO_top\"))
(def gen-nonprop-right (gen/return \"TODO_right\"))
(def gen-nonprop-bottom (gen/return \"TODO_bottom\"))
(def gen-nonprop-border-width (gen/return \"20\"))
(def gen-nonprop-border-style (gen/return \"TODO_border_style\"))
(def gen-nonprop-outline-radius (gen/return \"TODO_outline_radius\"))
(def gen-nonprop-outline-color (gen/return \"TODO_outline_color\"))
(def gen-nonprop-outline-style (gen/return \"TODO_outline_style\"))
(def gen-nonprop-outline-width (gen/return \"50\"))
(def gen-nonprop-x (gen/return \"11\"))
(def gen-nonprop-y (gen/return \"11\"))
(def gen-nonprop-time (gen/return \"TODO_time\"))
(def gen-nonprop-grid-template (gen/return \"TODO_grid_template\"))
(def gen-nonprop-grid-template-rows (gen/return \"TODO_grid_template_rows\"))
(def gen-nonprop-grid-template-columns (gen/return \"TODO_grid_template_columns\"))
(def gen-nonprop-grid-auto-rows (gen/return \"TODO_grid_auto_rows\"))
(def gen-nonprop-grid-auto-columns (gen/return \"TODO_grid_auto_columns\"))
(def gen-nonprop-grid-row-gap (gen/return \"TODO_grid_row_gap\"))
(def gen-nonprop-grid-column-gap (gen/return \"TODO_grid_column_gap\"))
(def gen-nonprop-column-width (gen/return \"TODO_column_width\"))
(def gen-nonprop-column-count (gen/return \"TODO_column_count\"))
(def gen-nonprop-column-rule-width (gen/return \"TODO_column_rule_width\"))
(def gen-nonprop-column-rule-style (gen/return \"TODO_column_rule_style\"))
(def gen-nonprop-column-rule-color (gen/return \"TODO_column_rule_color\"))
(def gen-nonprop-font-style (gen/return \"TODO_font_style\"))
(def gen-nonprop-font-weight (gen/return \"TODO_font_weight\"))
(def gen-nonprop-font-stretch (gen/return \"TODO_font_stretch\"))
(def gen-nonprop-font-size (gen/return \"TODO_font_size\"))
(def gen-nonprop-line-height (gen/return \"TODO_line_height\"))
(def gen-nonprop-font-family (gen/return \"TODO_font_family\"))
(def gen-nonprop-list-style-type (gen/return \"TODO_list_style_type\"))
(def gen-nonprop-list-style-position (gen/return \"TODO_list_style_position\"))
(def gen-nonprop-list-style-image (gen/return \"TODO_list_style_image\"))
(def gen-nonprop-resolution (gen/return \"TODO_resolution\"))
(def gen-nonprop-margin-left (gen/return \"TODO_margin_left\"))
(def gen-nonprop-min-width (gen/return \"TODO_min_width\"))
(def gen-nonprop-text-decoration-line (gen/return \"TODO_text-decoration-line\"))
(def gen-nonprop-text-decoration-style (gen/return \"TODO_text-decoration-style\"))
(def gen-nonprop-text-decoration-color (gen/return \"TODO_text-decoration-color\"))
(def gen-nonprop-background-color (gen/return \"TODO_background-color\"))
(def gen-nonprop-declaration-list (gen/return \"TODO_declaration_list\"))
(def gen-nonprop-declaration-value (gen/return \"TODO_declaration_value\"))
(def gen-nonprop-name-repeat (gen/return \"TODO_name_repeat\"))
(def gen-nonprop-flex-grow (gen/return \"TODO_flex_grow\"))
(def gen-nonprop-flex-shrink (gen/return \"TODO_flex_shrink\"))
(def gen-nonprop-flex-basis (gen/return \"TODO_flex_basis\"))
(def gen-nonprop-function-token (gen/return \"TODO_flex_function_token\"))
(def gen-nonprop-any-value (gen/return \"TODO_any_value\"))
(def gen-nonprop-offset-position (gen/return \"TODO_offset_position\"))
(def gen-nonprop-offset-path (gen/return \"TODO_offset_path\"))
(def gen-nonprop-offset-distance (gen/return \"TODO_offset_distance\"))
(def gen-nonprop-offset-rotate (gen/return \"TODO_offset_rotate\"))
(def gen-nonprop-offset-anchor (gen/return \"TODO_offset_anchor\"))
(def gen-nonprop-attr-name (gen/return \"TODO_attr_name\"))
(def gen-nonprop-attr-fallback (gen/return \"TODO_attr_fallback\"))
(def gen-nonprop-clip-style (gen/return \"TODO_clip_style\"))
(def gen-nonprop-frequency (gen/return \"TODO_frequency\"))
(def gen-nonprop-an-plus-b  (gen/return \"TODO_an_plus_b\"))

(def gen-nonprop-mask-image (gen/return \"TODO_mask_image\"))
(def gen-nonprop-mask-repeat (gen/return \"TODO_mask_repeat\"))
(def gen-nonprop-mask-attachment (gen/return \"TODO_mask_attachment\"))
(def gen-nonprop-mask-origin (gen/return \"TODO_mask_origin\"))
(def gen-nonprop-mask-clip (gen/return \"TODO_mask_clip\"))

(def gen-func-path (gen/return \"TODO_func_path\"))

;; Generated generators

"))

(defn value-generator-def [k v]
  (str "(def gen-" (generator-name k) "\n"
       (single-pipe (drop 1 v) 1) ")"))

(defn assignment-generator [m]
  (str "(def gen-css-assignment\n"
       "  (gen/frequency [\n"
       (string/join
         "\n"
         (for [[k v] m
               :when (= \' (first k))
               :let [p (string/replace k #"'" "")]]
           (str "    [100 (gen/tuple (gen/return \"" p ": \")"
                " gen-" (generator-name k) ")]")))
       "\n    ]))"))

(defn map->ns [ns m]
  (let [prop-deps (util/tree-deps m)
        prop-order (reverse (kahn/kahn-sort-throws prop-deps))
        gen-strs (for [p prop-order]
                   (value-generator-def p (get m p)))
        ]
    (str (prefix ns)
         (string/join "\n\n" gen-strs)
         "\n\n"
         (assignment-generator m)
         "\n\n"
         "(def gen-css-assignments\n"
         "  (gen/fmap #(util/flatten-text % \" \")\n"
         "    (gen/fmap #(interpose \"; \" %)\n"
         "              (gen/vector gen-css-assignment))))\n"
         "\n\n"
         )))

(comment
  ;; Takes 4 seconds
  (def css-tree (css3-syntax-parser (slurp "data/css3.pvs")))
  (def css-map (parsed-tree->map css-tree))

  ;; The following takes 25 seconds
  (def css-ns (map->ns "rend.css-generators" css-map))
  (spit "src/rend/css_generators.clj" css-ns)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(def cli-options
  [[nil "--pvs-output" "Path for storing copy of full PVS syntax file"
    :default "./data/css3.pvs"]
   [nil "--namespace NAMESPACE" "Name of namespace to generate"
    :default "test.css-generators"]])

(defn opt-errors [opts]
  (when (:errors opts)
    (map pr-err (:errors opts))
    (System/exit 2))
  opts)

(defn css-ns [nsname pvs-output]
  (let [pvs-text (pvs css3-properties css3-syntaxes)
        ;; Takes 4 seconds
        _ (pr-err "Creating full CSS PVS grammar file")
        css-tree (css3-syntax-parser pvs-text)
        css-map (parsed-tree->map css-tree)

        ;; The following takes 20+ seconds
        _ (pr-err "Converting CSS PVS grammars to generators")
        css-ns (map->ns nsname css-map)]
    (when pvs-output
      (pr-err "Saving full CSS PVS grammar file to:" pvs-output)
      (spit pvs-output pvs-text))
    css-ns))

(defn -main [& args]
  (let [opts (:options (opt-errors (parse-opts args cli-options)))]
    (println (css-ns (:namespace opts) (:pvs-output opts)))))

(comment
  ;; time lein with-profile css3 run --namespace rend.css3-generators > src/rend/css3_generators.clj

  (require '[rend.css3-generators :as css3-gen] :reload)
  (pprint (gen/sample css3-gen/gen-css-assignments 10))
)

