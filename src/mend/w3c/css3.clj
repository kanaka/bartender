(ns mend.w3c.css3
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]

            [instaparse.core :as insta]))

;; TODO: global properties (inherit, initial, unset, revert)

;; https://developer.mozilla.org/en-US/docs/Web/CSS/Value_definition_syntax
;; https://www.smashingmagazine.com/2016/05/understanding-the-css-property-value-syntax/
;; https://www.w3.org/TR/CSS21/grammar.html
;; https://github.com/mdn/data/tree/master/css
;; https://github.com/csstree/csstree/
;; https://csstree.github.io/docs/syntax.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn css-vds-combined [properties syntaxes]
  (let [syns (mangle-css-syntaxes syntaxes)
        ps (for [[prop {:strs [syntax]}] (sort properties)]
             (str "<'" prop "'> = " syntax "\n"))
        ss (for [[syn syntax] (sort syns)]
             (str "<" syn "> = " syntax "\n"))]
    (apply str (concat ps ss))))

(comment
  (spit "data/css3.vds" (css-vds-combined
                          (filter-css-properties
                            (json/read-str (slurp "./mdn_data/css/properties.json")))
                          (json/read-str (slurp "mdn_data/css/syntaxes.json"))))
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
  (string/join
    "\n\n"
    ["css-assignments = S css-assignment S (';' S css-assignment)* S"
     (assignment-ebnf m)
     (string/join
       " ;\n\n"
       (for [[k v] m] (value-ebnf k v)))]))

(comment
  ;; Takes 4 seconds
  (def css-tree (css3-syntax-parser (slurp "data/css3.vds")))
  (def css-map (parsed-tree->map css-tree))

  ;; The following takes 25 seconds
  (def css-ebnf (map->ebnf css-map))
  (spit "data/css3.ebnf" css-ebnf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(def cli-options
  [[nil "--vds-grammar EBNF-OUTPUT"
    "Path to generic VDS grammar definition."
    :default "./data/css-vds.ebnf"]
   [nil "--css-vds-output CSS-VDS-OUTPUT"
    "Write full CSS VDS syntax to file"
    :default "./data/css3.vds"]
   [nil "--ebnf-output EBNF-OUTPUT"
    "Write intermediate EBNF to file"
    :default "./data/css3.ebnf"]
   [nil "--css-properties CSS-PROPERTIES"
    "Path to CSS properties JSON file."
    :default "./mdn_data/css/properties.json"]
   [nil "--css-syntaxes CSS-SYNTAXES"
    "Path to CSS syntaxes JSON file."
    :default "mdn_data/css/syntaxes.json"]
   [nil "--ebnf-prefix EBNF-PREFIX"
    "Path to prefix file to include in EBNF output"
    :default "./data/css3-prefix.ebnf"]
   [nil "--ebnf-base EBNF-BASE"
    "Path to base grammar file to include in EBNF output"
    :default "./data/css3-base.ebnf"]
   [nil "--filter-status FILTER-STATUS"
    "Name of namespace to generate"]
   ])

(defn ebnf-combined-str [css-map opts]
  (string/join
    "\n\n"
    ["(* Generated by mend.w3c.css3 *)"
     (slurp (:ebnf-prefix opts))
     (map->ebnf css-map)
     (slurp (:ebnf-base opts))]))


(defn -main [& args]
  "Generate a CSS 3 EBNF grammar based on specification data from
  Mozilla Developer Network (MDN).

  This takes about 11 seconds to run."
  (let [opts (:options (opt-errors (parse-opts args cli-options)))

        _ (pr-err "Creating VDS grammar parser from:" (:vds-grammar opts))
        css3-syntax-parser (insta/parser (:vds-grammar opts))

        properties-file (:css-properties opts)
        syntaxes-file (:css-syntaxes opts)
        _ (pr-err "Generating full CSS VDS grammar based on:"
                  properties-file syntaxes-file)
        properties (filter-css-properties
                     (json/read-str (slurp properties-file))
                     (:filter-status opts))
        syntaxes (json/read-str (slurp syntaxes-file))
        vds-text (css-vds-combined properties syntaxes)

        _ (when-let [pfile (:css-vds-output opts)]
            (pr-err "Saving full CSS VDS grammar file to:" pfile)
            (spit pfile vds-text))

        _ (pr-err "Parsing CSS VDS grammar")
        css-tree (css3-syntax-parser vds-text)
        css-map (parsed-tree->map css-tree)

        _ (pr-err "Converting CSS VDS grammar to EBNF")
        css3-ebnf-str (ebnf-combined-str css-map opts)]

    (println "Saving EBNF to" (:ebnf-output opts))
    (spit (:ebnf-output opts) css3-ebnf-str)

    (println "Checking CSS3 EBNF grammar")
    ;; The following takes 5 seconds
    (insta/parser css3-ebnf-str)))

