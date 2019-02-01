(ns mend.css3
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]

            [instaparse.core :as insta]
            [clojure.tools.cli :refer [parse-opts]]

            [instacheck.core :as ebnf]
            [instacheck.cli :as ebnf-cli]

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

(def CSS3-PROPERTIES "mdn_data/css/properties.json")
(def CSS3-SYNTAXES "mdn_data/css/syntaxes.json")

(def css3-syntax-parser (insta/parser (slurp "data/css-vds.ebnf")))

;; Find each :path in a grammar and replace it with :value
;; This allows us to replace stub generators in the grammar with
;; references to generators in a different namespace as just one
;; example.
(def simple-grammar-updates
  [;; Replace regex number generators with actual numeric/sized types
   {:path [:nonprop-integer]
    :value {:tag :nt :keyword :gen/int}}
   {:path [:nonprop-positive-integer]
    :value {:tag :nt :keyword :gen/pos-int}}
   {:path [:number-float]
    :value {:tag :nt :keyword :gen/double}}])

(defn prune-S [x]
  (if (and (:parsers x)
           (> (count (:parsers x)) 1))
    (assoc x :parsers (filter #(not= (:keyword %) :S)
                              (:parsers x)))
    x))

(defn grammar-update-fn [ctx grammar]
  (let [g1 (ebnf/apply-grammar-updates grammar simple-grammar-updates)
        ;; Remove empty strings
        g2 (postwalk prune-S g1)]
    g2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn filter-css-properties
  "Filter properties we want (also removes '--*')"
  [props & [status]]
  (into {} (filter (fn [[name attrs]]
                     (and
                       (re-find #"^[a-z-]+$" name)
                       (or (not status)
                           (= (get attrs "status") status))))
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

(defn vds-all [properties syntaxes]
  (let [syns (mangle-css-syntaxes syntaxes)
        ps (for [[prop {:strs [syntax]}] (sort properties)]
             (str "<'" prop "'> = " syntax "\n"))
        ss (for [[syn syntax] (sort syns)]
             (str "<" syn "> = " syntax "\n"))]
    (apply str (concat ps ss))))

(comment
  (spit "data/css3.vds" (vds-all (filter-css-properties
                                   (json/read-str (slurp CSS3-PROPERTIES)))
                                 (json/read-str (slurp CSS3-SYNTAXES))))
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

(comment
  ;; Takes 4 seconds
  (def parse-tree (css3-syntax-parser (slurp "data/css3.vds")))
  (def parse-map (parsed-tree->map parse-tree))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from: https://www.w3.org/TR/CSS21/grammar.html
(def ebnf-prefix "
(* Generated by mend.css3 *)

stylesheet = S css-ruleset* ;

css-ruleset = css-selector [ ',' S css-selector ]* S '{' S css-assignments S '}' S ;

css-selector = css-simple-selector [ css-combinator css-selector | S+ [ css-combinator? css-selector ]? ]? ;

css-simple-selector = css-element-name [ css-hash | css-class | css-pseudo ]*
                    | [ css-hash | css-class | css-pseudo ]+ ;

css-combinator = '+' S
               | '>' S ;

css-element-name = IDENT
                 | '*' ;

css-hash   = '#' NAME ;
css-class  = '.' IDENT ;
css-pseudo = ':' [ IDENT | IDENT '(' S [IDENT S]? ')' ] ;
NAME       = #'[_a-z0-9-]+' ;
IDENT      = #'-?[_a-z][_a-z0-9-]*' ;
S          = #'\\s*' ;
rS         = #'\\s*' ;

")

(def ebnf-suffix "

(* Base Types: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Types *)

nonprop-angle = nonprop-number ( 'deg' | 'grad' | 'rad' | 'turn' ) ;
(* nonprop-basic-shape = ; *)
(* nonprop-blend-mode = ; *)
(* nonprop-color = ; *)
(* TODO: excluded values: https://developer.mozilla.org/en-US/docs/Web/CSS/custom-ident *)
nonprop-custom-ident = ( #'[A-Za-z0-9_-]' | #'\\\\[0-9A-F][0-9A-F]?[0-9A-F]?[0-9A-F]?[0-9A-F]?[0-9A-F]?' )+ ;
nonprop-flex = nonprop-number 'fr' ;
nonprop-frequency = nonprop-number ( 'Hz' | 'kHz' ) ;
(* nonprop-gradient = ; *)
(* nonprop-image = ; *)
nonprop-integer = '-'? #'[0-9]+' ;
nonprop-length = nonprop-number nonprop-length-unit
               | '0' ;
nonprop-number = nonprop-integer | number-float ;
nonprop-percentage = nonprop-number '%' ;
(* nonprop-position = ; *)
nonprop-ratio = nonprop-positive-integer '/' nonprop-positive-integer ;
nonprop-resolution = nonprop-number ( 'dpi' | 'dpcm' | 'dppx' ) ;
(* nonprop-shape = ; *)
(* TODO: improve strings: https://developer.mozilla.org/en-US/docs/Web/CSS/string *)
nonprop-string = \"'\" #\"[A-Za-z0-9 _\\\"-]*\" \"'\"
               | '\"' #\"[A-Za-z0-9 _'-]*\" '\"' ;
nonprop-time = nonprop-number ( 's' | 'ms' ) ;
(* nonprop-transform-function = ; *)
(* TODO: improve urls: https://developer.mozilla.org/en-US/docs/Web/CSS/url *)
nonprop-url = 'url(http://STUB_DOMAIN.com/STUB_PATH/STUB_IMAGE=.png)' ;

(* Convenenience rules for types above *)

nonprop-positive-integer = #'[0-9]+' ;
number-float = '-'? #'[0-9]+' '.' #'[0-9]+' ;
nonprop-length-unit = 'em' | 'ex' | 'ch' | 'ic' | 'rem' | 'lh' | 'rlh'
                    | 'vh' | 'vw' | 'vi' | 'vb' | 'vmin' | 'vmax'
                    | 'px' | 'mm' | 'q' | 'cm' | 'in' |  'pt' | 'pc' | 'mozmm' ;


(* TODO: fix. These have similarly name prop-* rules *)

nonprop-border-radius = prop-border-radius ;
nonprop-padding-left = prop-padding-left ;

nonprop-top = prop-top ;
nonprop-bottom = prop-bottom ;
nonprop-left = prop-left ;
nonprop-right = prop-right ;

nonprop-background-color = prop-background-color ;

nonprop-border-width = prop-border-width ;
nonprop-border-style = prop-border-style ;
nonprop-border-radius = prop-border-radius ;
nonprop-border-image-source = prop-border-image-source ;
nonprop-border-image-slice = prop-border-image-slice ;
nonprop-border-image-width = prop-border-image-width ;
nonprop-border-image-outset = prop-border-image-outset ;
nonprop-border-image-repeat = prop-border-image-repeat ;

nonprop-column-width = prop-column-width ;
nonprop-column-count = prop-column-count ;
nonprop-column-rule-width = prop-column-rule-width ;
nonprop-column-rule-style = prop-column-rule-style ;
nonprop-column-rule-color = prop-column-rule-color ;

nonprop-flex = prop-flex ;
nonprop-flex-direction = prop-flex-direction ;
nonprop-flex-wrap = prop-flex-wrap ;
nonprop-flex-grow = prop-flex-grow ;
nonprop-flex-shrink = prop-flex-shrink ;
nonprop-flex-basis = prop-flex-basis ;

nonprop-font-style = prop-font-style ;
nonprop-font-weight = prop-font-weight ;
nonprop-font-stretch = prop-font-stretch ;
nonprop-font-size = prop-font-size ;
nonprop-font-family = prop-font-family ;

nonprop-grid-template = prop-grid-template ;
nonprop-grid-template-rows = prop-grid-template-rows ;
nonprop-grid-template-columns = prop-grid-template-columns ;
nonprop-grid-auto-rows = prop-grid-auto-rows ;
nonprop-grid-auto-columns = prop-grid-auto-columns ;
nonprop-grid-row-gap = prop-grid-row-gap ;
nonprop-grid-column-gap = prop-grid-column-gap ;

nonprop-line-height = prop-line-height ;

nonprop-list-style-type = prop-list-style-type ;
nonprop-list-style-position = prop-list-style-position ;
nonprop-list-style-image = prop-list-style-image ;

nonprop-margin-left = prop-margin-left ;

nonprop-mask-image = prop-mask-image ;
nonprop-mask-repeat = prop-mask-repeat ;
nonprop-mask-origin = prop-mask-origin ;
nonprop-mask-clip = prop-mask-clip ;

nonprop-outline-color = prop-outline-color ;
nonprop-outline-style = prop-outline-style ;
nonprop-outline-width = prop-outline-width ;

nonprop-text-emphasis-style = prop-text-emphasis-style ;
nonprop-text-emphasis-color = prop-text-emphasis-color ;
nonprop-text-decoration-line = prop-text-decoration-line ;
nonprop-text-decoration-style = prop-text-decoration-style ;
nonprop-text-decoration-color = prop-text-decoration-color ;

nonprop-width = prop-width ;
nonprop-max-width = prop-max-width ;
nonprop-min-width = prop-min-width ;


(* Other Misc Missing
   TODO: figure out why? *)

nonprop-hex-color3 = '#' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' ;
nonprop-hex-color6 = '#' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' #'[0-9A-Fa-f]' ;
nonprop-hex-color = nonprop-hex-color3
                  | nonprop-hex-color6 ;

nonprop-id-selector = nonprop-custom-ident ;

func-path = \"STUB_func_path\" ;
nonprop-an-plus-b = \"STUB_an_plus_b\" ;
nonprop-any-value = \"STUB_any_value\" ;
nonprop-attr-name = \"STUB_attr_name\" ;
nonprop-attr-fallback = \"STUB_attr_fallback\" ;
nonprop-clip-style = \"STUB_clip_style\" ;
nonprop-custom-property-name = \"STUB_custom_property\" ;
nonprop-declaration-list = \"STUB_declaration_list\" ;
nonprop-declaration-value = \"STUB_declaration_value\" ;
nonprop-dimension = \"STUB_dimension\" ;
nonprop-function-token = \"STUB_flex_function_token\" ;
nonprop-ident = nonprop-custom-ident ;
nonprop-mask-attachment = \"STUB_mask_attachment\" ;
nonprop-name-repeat = \"STUB_name_repeat\" ;
nonprop-outline-radius = \"STUB_outline_radius\" ;
nonprop-x = \"11\" ;
nonprop-y = \"11\" ;

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare juxtapose-ebnf double-amp-ebnf double-bar-ebnf component-ebnf
         component-single-ebnf component-multiplied-ebnf brackets-ebnf
         block-ebnf func-ebnf braces-ebnf)

;; Operator precendence summary:
;;  mult, juxt, &&, ||, |
;;
;; Notes:
;; - juxtaposition has precedence over the double ampersand, meaning
;;   that
;;     bold thin && <length> is equivalent to
;;     [ bold thin ] && <length>
;; - the double ampersand has precedence over the double bar, meaning
;;   that
;;     bold || thin && <length> is equivalent to
;;     bold || [ thin && <length> ]
;; - the double bar has precedence over the single bar, meaning that
;;     bold | thin || <length> is equivalent to
;;     bold | [ thin || <length> ]
;; - multipliers cannot be added and have all precedence over
;; combinators.
;;

(defn name-ebnf [k]
  (cond
    (= \' (first k))
    (str "prop-" (string/replace k #"'" ""))

    (re-find #"\(\)$" k)
    (str "func-" (string/replace k #"\(\)$" ""))

    :else
    (str "nonprop-" k)))

(defn single-bar-ebnf
  "One of the values must occur."
  [tree indent]
  ;;(prn :** :single-bar-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (double-bar-ebnf (drop 1 (first tree)) indent)
      (str pre "(\n"
           (string/join
             (str " |\n")
             (for [t tree]
               (double-bar-ebnf (drop 1 t) (+ 1 indent))))
           "\n"
           pre ")"))))

;; TODO: for EBNF we just treat this like a single-bar with '+'
;; appended. This means that we may get more than one of each
;; element.
(defn double-bar-ebnf
  "One or more of the values must occur in any order."
  [tree indent]
  ;;(prn :** :double-bar-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (double-amp-ebnf (drop 1 (first tree)) indent)
      (str pre "( (\n"
           (string/join
             (str " |\n")
             (for [t tree]
               (double-amp-ebnf (drop 1 t) (+ 1 indent))))
           "\n"
           pre ")+ )"))))

;; TODO: for EBNF we just treat this as juxtapose we means we use the
;; original order it is defined with. Fix this to allow any order.
(defn double-amp-ebnf
  "All values must occur in any order."
  [tree indent]
  ;;(prn :** :double-amp-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (juxtapose-ebnf (drop 1 (first tree)) indent)
      (str pre "(\n"
           (string/join
             "\n"
             (for [t tree]
               (if (= :comma (first t))
                 (str pre "  ', '")
                 (juxtapose-ebnf (drop 1 t) (+ 1 indent)))))
           "\n"
           pre ")"))))

(defn juxtapose-ebnf
  "Each value must occur."
  [tree indent]
  ;;(prn :** :juxtapose-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count tree))
      (component-ebnf (second (first tree)) indent)
      (str pre "(\n"
           (string/join
             "\n"
             (for [t tree]
               (if (= :comma (first t))
                 (str pre "  ', '")
                 (component-ebnf (second t) (+ 1 indent)))))
           "\n"
           pre ")"))))

(defn component-ebnf [tree indent]
  ;;(prn :** :component-ebnf (count tree) tree :indent indent)
  (str
    "' ' "
    (condp = (first tree)
      :component-single     (component-single-ebnf (second tree) indent)
      :component-multiplied (component-multiplied-ebnf (drop 1 tree) indent))))

(defn component-single-ebnf [tree indent]
  ;;(prn :** :component-single-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))]
    (condp = (first tree)
      :literal       (str pre "'" (second tree) "'")
      :keyword-value (str pre "'" (second tree) "'")
      :non-property  (str pre (name-ebnf (second tree)))
      :property      (str pre (name-ebnf (second tree)))
      :brackets      (brackets-ebnf (drop 1 tree) indent)
      :block         (block-ebnf    (drop 1 tree) indent)
      :func          (func-ebnf     (drop 1 tree) indent)
      )))

(defn component-multiplied-ebnf [[tree multiplier] indent]
  ;;(prn :** :component-multiplied-ebnf (count tree) tree :indent indent)
  (let [pre (apply str (repeat indent "  "))
        single (partial component-single-ebnf (second tree))]
    (condp = (first (second multiplier))
      :question    (str (single indent) "?")
      :asterisk    (str (single indent) "*")
      :plus        (str (single indent) "+")
      :hash        (str (single indent) " (\n"
                        pre "  S ',' S\n"
                        (single (+ 1 indent)) "\n"
                        pre ")*")
      :braces      (braces-ebnf (second (second multiplier))
                                false (single (+ 2 indent)) indent)
      :hash-braces (braces-ebnf (second (second (second multiplier)))
                                true (single (+ 2 indent)) indent)
    )))

(defn brackets-ebnf [tree indent]
  ;;(prn :** :brackets-ebnf tree indent)
  ;; TODO: deal with bang?
  (single-bar-ebnf (drop 1 (first tree)) indent))

(defn func-ebnf [tree indent]
  ;;(prn :** :func-ebnf tree indent)
  (let [pre (apply str (repeat indent "  "))]
    (str pre "'" (first tree) "('\n"
         (single-bar-ebnf (drop 1 (second tree)) (+ 1 indent)) "\n"
         pre "')'")))

(defn block-ebnf [tree indent]
  ;;(prn :** :block-ebnf tree indent)
  (let [pre (apply str (repeat indent "  "))]
    (str pre "'{'\n"
         (juxtapose-ebnf (drop 1 (second tree)) (+ 1 indent)) "\n"
         pre "'}'")))

(defn braces-ebnf [kind hash? single indent]
  ;;(prn :** :braces-ebnf kind hash? single indent)
  (let [pre (apply str (repeat indent "  "))
        bmin (Integer. (second (second kind)))
        bmax (Integer. (second (nth kind 2 [:digit bmin])))]
    ;;(assert (= :bracesA-B (first kind))
    ;;        (str "Unsupported curly braces repeat form:" kind))
    (assert (and (>= bmin 0)
                 (>= bmax bmin)
                 (<= bmax 20))
            (str "Unsupported curly braces range: " bmin "-" bmax))
    (str pre "(\n"
         pre "  (\n"
         (string/join
           " |\n"
           (for [i (range bmin (+ 1 bmax))]
             (string/join
               (if hash? " ', '\n" "\n")
               (if (= i 0)
                 (str pre "''")
                 (repeat i single)))))
         "\n"
         pre "  )\n"
         pre ")")))
;;

(defn value-ebnf [k v]
  ;;(prn :value-ebnf :k k :v v)
  (str (name-ebnf k) " = \n"
       (single-bar-ebnf (drop 1 v) 1)))

(defn assignment-ebnf [m]
  (str "css-assignment =\n  (\n"
       (string/join
         " |\n"
         (for [[k v] m
               :when (= \' (first k))]
           (str "    \"" (string/replace k #"'" "")
                "\" S \":\" S " (name-ebnf k))))
       "\n  )"))

(defn map->ebnf [m]
  (let [ebnf-strs (for [[k v] m] (value-ebnf k v))]
    (str ebnf-prefix
         "css-assignments = S css-assignment S (';' S css-assignment)* S"
         "\n\n"
         (assignment-ebnf m)
         "\n\n"
         (string/join " ;\n\n" ebnf-strs)
         "\n\n"
         ebnf-suffix)))

(comment
  ;; Takes 4 seconds
  (def css-tree (css3-syntax-parser (slurp "data/css3.vds")))
  (def css-map (parsed-tree->map css-tree))

  ;; The following takes 25 seconds
  (def css-ebnf (map->ebnf css-map))
  (spit "data/css3.ebnf" css-ebnf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ns-prefix [ctx]
  (str
"(ns " (:namespace ctx) "
   (:require [clojure.test.check.generators :as gen]
             [com.gfredericks.test.chuck.generators :as chuck]))

;; Generated by mend.css3

"))

(defn grammar->ns
  [ctx grammar]
  (str (ns-prefix ctx)
       (if (:function ctx)
         (ebnf/grammar->generator-func-source ctx grammar)
         (ebnf/grammar->generator-defs-source ctx grammar))))

(def cli-options
  (vec
    (concat
      ebnf-cli/general-cli-options
      [[nil "--namespace NAMESPACE"
	"Name of namespace to generate"]
       [nil "--vds-output VDS-OUTPUT"
        "Path for storing copy of full VDS syntax file"
        :default "./data/css3.vds"]
       [nil "--ebnf-output EBNF-OUTPUT"
        "Write intermediate EBNF to file"]
       [nil "--filter-status FILTER-STATUS"
	"Name of namespace to generate"]
       [nil "--function FUNCTION"
        "Emit a function named FUNCTION which returns a generator rather than a defn for every generator"]])))

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(defn css3-ns [opts]
  (let [ctx (merge {:weights-res (atom {})
                    :grammar-updates grammar-update-fn}
                   (select-keys opts [:namespace
                                      :weights :function]))

        _ (pr-err "Generating full CSS VDS grammar from files:"
                  CSS3-PROPERTIES CSS3-SYNTAXES)
        css3-properties (json/read-str (slurp CSS3-PROPERTIES))
        css3-syntaxes (json/read-str (slurp CSS3-SYNTAXES))

        vds-text (vds-all (filter-css-properties css3-properties
                                                 (:filter-status opts))
                          css3-syntaxes)
        _ (when-let [pfile (:vds-output opts)]
            (pr-err "Saving full CSS VDS grammar file to:" pfile)
            (spit pfile vds-text))

        _ (pr-err "Loading CSS VDS grammar")
        css-tree (css3-syntax-parser vds-text)
        css-map (parsed-tree->map css-tree)

        _ (pr-err "Converting CSS VDS grammar to EBNF")
        css3-ebnf-str (map->ebnf css-map)
        _ (when-let [efile (:ebnf-output opts)]
            (pr-err "Saving EBNF to" efile)
            (spit efile css3-ebnf-str))

        _ (pr-err "Loading CSS grammar from EBNF")
        ;; The following takes 5 seconds
        css3-grammar (ebnf/load-grammar css3-ebnf-str)

        _ (pr-err "Converting CSS grammar to source")
        ;; The following takes 14+ seconds
        ns-str (grammar->ns ctx css3-grammar)]

    (when-let [wfile (:weights-output opts)]
      (pr-err "Saving weights to" wfile)
      (ebnf-cli/save-weights ctx (:weights-output opts)))

    ns-str))


(defn -main [& args]
  (let [opts (:options (opt-errors (parse-opts args cli-options)))]
    (when (not (:namespace opts))
      (pr-err "--namespace NAMESPACE required")
      (System/exit 2))

    (println (css3-ns opts))))

