(ns wend.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [hickory.core]
            [hickory.render]
            [hickory.select :as s]
            [instacheck.core :as instacheck]
            [instaparse.core]
            [instaparse.print]))

(def EBNF-PATHS
  {:html-gen     ["html5-test.ebnf"
                  "html5.ebnf"]
   :html-gen-min ["html5-test.ebnf"
                  "html5.ebnf"]
   :html-parse   ["html5.ebnf"]
   :css-gen      ["css3-test.ebnf"
                  "css3.ebnf"]
   :css-parse    ["css3.ebnf"]})

(def GRAMMAR-MANGLES
  {:html-gen     {}
   :html-gen-min {}
   :html-parse   {:char-data :char-data-generic
                  :comment :comment-generic
                  :url :url-generic}
   :css-gen      {}
   :css-parse    {:nonprop-group-rule-body :stylesheet
                  :prop-group-rule-body :css-ruleset
                  :nonprop-declaration-list :css-assignments}})

(def START-RULES
  {:html-gen     :html-test
   :html-gen-min :html-test-min
   :html-parse   :html
   :css-gen      :css-assignments-test
   :css-parse    :stylesheet})

(defn mangle-parser
  [parser mangles]
  (reduce (fn [p [k v]] (assoc-in p [:grammar k]
                                  {:tag :nt, :keyword v
                                   :red {:reduction-type :hiccup, :key k}}))
          parser mangles))

(defn load-parser* [paths mangles]
  (let [ebnf (string/join "\n" (map slurp (map io/resource paths)))
        base-parser (instaparse.core/parser ebnf)
        parser (mangle-parser base-parser mangles)]
    parser))

(defn load-parser [kind]
  (let [parser (load-parser* (get EBNF-PATHS kind) (get GRAMMAR-MANGLES kind))]
    (assoc parser :start-production (get START-RULES kind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def PRUNE-TAGS
  #{:style
    :script
    :svg})

(def PRUNE-TAGS-BY-ATTRIBUTE
  ;; [tag attribute]
  #{[:meta :property]})

(def PRUNE-TAGS-BY-ATTRIBUTE-VALUE
  ;; [tag attribute value]
  #{[:link :rel "stylesheet"]})

(def PRUNE-ATTRIBUTES
  #{:style
    :x-ms-format-detection
    :data-viewport-emitter-state
    :windowdimensionstracker})

(def PRUNE-TAG-ATTRIBUTES
  ;; tag -> [attribute ...]
  {:input [:autocapitalize :autocorrect]
   :link [:as]
   :iframe [:frameborder :scrolling]
   :div [:type]
   :span [:for :fahrenheit]

   ;; TODO: these are HTML 5+ and shouldn't be removed when parsing
   ;; that.
   :video [:playsinline :webkit-playsinline]
   })

(def REWRITE-TAG-ATTRIBUTE-VALUES
  ;; [tag attribute value new-value]
  {[:select :required "required"] "true"})


(defn prune-tags
  "Takes hickory and prunes tags"
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (vector? n)
              (vec (remove #(or (contains? PRUNE-TAGS (:tag %))
                                (seq (set/intersection
                                       PRUNE-TAGS-BY-ATTRIBUTE
                                       (set (map vector
                                                 (repeat (:tag %))
                                                 (keys (:attrs %))))))
                                (seq (set/intersection
                                       PRUNE-TAGS-BY-ATTRIBUTE-VALUE
                                       (set (map vector
                                                 (repeat (:tag %))
                                                 (keys (:attrs %))
                                                 (vals (:attrs %)))))))
                           n))
              n))
    h))

(defn prune-attributes
  "Takes hickory and prunes attributes from all tags"
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (-> n :attrs)
                 ;; i.e. (dissoc-in n [:attrs :style])
                 (assoc n :attrs (apply dissoc (:attrs n)
                                        PRUNE-ATTRIBUTES))
                 n))
    h))

(defn prune-tag-attributes
  "Takes hickory and prunes matching tag+attributes combinations"
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (contains? PRUNE-TAG-ATTRIBUTES (:tag n))
              (assoc n :attrs
                     (apply dissoc (:attrs n)
                            (get PRUNE-TAG-ATTRIBUTES (:tag n))))
              n))
    h))

(defn rewrite-tag-attribute-values
  "Takes hickory and rewrites matcing tag+attribute+values to a new value"
  [h]
  (let [rtav  REWRITE-TAG-ATTRIBUTE-VALUES]
    (clojure.walk/prewalk
      (fn [n] (reduce
                (fn [node x]
                  (assoc-in node [:attrs (second x)] (get rtav x)))
                n
                (set/intersection
                  (set (keys rtav))
                  (set (map vector
                            (repeat (:tag n))
                            (keys (:attrs n))
                            (vals (:attrs n)))))))
      h)))

(defn cleanup-ws-in-attrs
  "Takes hickory and removes leading and traling extraneous whitespace
  in tag attributes."
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (map? (-> n :attrs))
              (assoc n :attrs
                     (into {} (for [[k v] (-> n :attrs)]
                                [k
                                 (if (string? v) (string/trim v) v)])))
              n))
    h))

(defn extract-html
  "Returns cleaned up and normalized HTML text strings. Specifically
  it converts HTML to hickory, removes style tags and attributes,
  applies some other manual cleanups, then converts back to HTML in
  order to normalize the HTML into a more standard and consistent
  form."
  [html]
  (-> html
      ;; Remove unicode characters
      (string/replace #"[^\x00-\x7f]" "")
      hickory.core/parse
      hickory.core/as-hickory
      prune-tags
      prune-attributes
      prune-tag-attributes
      rewrite-tag-attribute-values
      cleanup-ws-in-attrs
      hickory.render/hickory-to-html
      ;; Translate \xc9 unicode character back to char reference so it
      ;; can be parsed by our parser.
      (string/replace #"[\xc9]" "&#x00c9;")))

(defn cleanup-css
  [css]
  (-> css
      ;; Remove unicode characters
      (string/replace #"[^\x00-\x7f]" "")
      ;; Remove non-unix newlines
      (string/replace #"[\r]" "\n")
      ;; remove vendor prefixes
      (string/replace #"([^A-Za-z0-9])(?:-webkit-|-moz-|-ms-)" "$1")
      ;; Remove apple specific CSS property
      (string/replace #"x-content: *\"[^\"]*\"" "")
      ;; Some at-rule syntax require semicolons before closing curly
      (string/replace #"(@font-face *[{][^}]*[^;])[}]" "$1;}")
      ))

;;        ;; remove vendor prefixes (from at-rules)
;;        #"@(-webkit-|-moz-|-ms-)" "@")
;;      ;; Remove vendor prefixes (-webkit-, -moz-, -ms-) from some properties
;;      #"(-webkit-|-moz-|-ms-)(transform|text-size-adjust|box-sizing|font-feature-settings)\b" "$2"

(defn extract-inline-css
  "Return text of all inline styles. Specifically it converts the HTML
  file to hickory, extracts the content from style tags and
  concatenates it together."
  [html]
  (let [h (-> html
              hickory.core/parse
              hickory.core/as-hickory)
        ;; Extract inline tag specific styles
        styles (map #(->> % :attrs :style)
                    (s/select (s/child (s/attr :style)) h))
        ;; Remove trailing semis, then join with single semi+newline
        style (string/join ";\n" (map #(string/replace % #";\s*$" "")
                                      styles))]
    style))

(defn extract-css-map
  "Return a map of CSS texts with the following keys:
  - :inline-style  -> all inline styles in
                      a wildcard selector (i.e. '* { STYLES }')
  - :inline-sheet-X -> inline stylesheets by indexed keyword
  - \"sheet-href\"  -> loaded stylesheets by path/URL

  The returned styles can be combined into a single stylesheet like this:
      (clojure.string/join \"\n\" (apply concat CSS-MAP))"
  [html & [base-path]]
  (let [h (-> html
              hickory.core/parse
              hickory.core/as-hickory)
        ;; Extract inline tag specific styles
        styles (map #(->> % :attrs :style)
                    (s/select (s/child (s/attr :style)) h))
        inline-style (str
                       "* {\n    "
                       (string/join "\n    " styles)
                       "\n}")
        ;; Extract inline stylesheets
        inline-sheets (map #(->> % :content (apply str))
                           (s/select (s/child (s/tag :style)) h))
        ;; Load linked stylesheets from files
        link-tags (s/select (s/child (s/and
                                       (s/tag :link)
                                       (s/attr :rel
                                               #(= "stylesheet" %)))) h)
        sheet-hrefs (map #(->> % :attrs :href (io/file base-path))
                         link-tags)
        loaded-sheets (map #(str "/* from: " % " */\n"
                                 (slurp %))
                           sheet-hrefs)
        css-map (merge {:inline-style (cleanup-css inline-style)}
                       (zipmap (map (comp keyword str)
                                    (repeat "inline-sheet-")
                                    (range))
                               (map cleanup-css inline-sheets))
                       (zipmap (map str sheet-hrefs)
                               (map cleanup-css loaded-sheets)))]
    css-map))

(comment

(def css-map (extract-css-map (slurp "test/html/example.com-20190422.html") "test/html"))
(print (string/join "\n\n" (vals css-map)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar weight functions

;; TODO: these generic functions should move to instacheck

(defn save-weights [path weights]
  (io/make-parents path)
  (let [sm (sorted-map-by #(compare (str %1) (str %2)))]
    (spit path (with-out-str (pprint (into sm weights))))))

(defn parse-weights [parser texts]
  (let [texts (if (string? texts) [texts] texts)
        parsed (map #(instacheck/parse parser %) texts)]
    (apply merge-with +
           (map #(frequencies (-> % meta :path-log)) parsed))))

(defn adjust-weights
  "Returns a subset of base-weights with weight values adjusted by the
  adjuster function. For a weight to be adjusted, it must satisfy the
  following criteria:
  - the path must present in both adjust-weights and base-weights
  - the path must also be present in base-weights
  - the path must specify an alternation (e.g. [... :alt X])
  - the path must satisfy the adjustable? function
  If the criteria are satisfied then the path is added to the returned
  map with the weight value from base-weights adjusted by the adjuster
  function.
  "
  [adjustable? adjuster base-weights adjust-weights]
  (let [alt-weights (instacheck/filter-alts adjust-weights)
        adjusted (for [[p w] alt-weights
                       :when (adjustable? base-weights p w)]
                   [p (adjuster (get base-weights p))])]
    (into {} adjusted)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar/weight path functions

(defn grammar-node
  "Get the a grammar node for the given path in grammar. Nil is
  returned if the path does not exist in the grammar, the tag type
  along the path don't match, or the numeric parser index is
  out-of-bounds."
  [grammar path]
  (loop [g (get grammar (first path))
         p (rest path)]
    (let [[t1 t2 & ts] p]
      (cond
        (empty? p)
        g

        (or (nil? g) (not= (:tag g) t1))
        nil

        (and (number? t2) (:parsers g) (> (count (:parsers g)) t2))
        (recur (nth (:parsers g) t2) ts)

        (:parser g)
        (recur (:parser g) (rest p))

        :else
        nil))))

(defn grammar-path*
  "Given a grammar for a rule and a grammar path (not including the
  rule key), prune the grammar to include only the alt values
  specified in the grammar path."
  [grammar path]
  (let [[g p] [grammar path]
        [t1 t2 & ts] p]
    ;;(prn :t1 t1 :t2 t2 :alt? (= t1 :alt)
    ;;     :parsers? (contains? g :parsers)
    ;;     :parser? (contains? g :parser))
    (cond
      (empty? p)
      g

      (= t1 :alt)
      (grammar-path* (nth (:parsers g) t2) ts)

      (= t1 :cat)
      (assoc g :parsers (map-indexed (fn [idx itm]
                                       (if (= t2 idx)
                                         (grammar-path* itm ts)
                                         itm))
                                     (:parsers g)))

      (:parser g)
      (assoc g :parser (grammar-path* (:parser g) (rest p)))

      :else
      (throw (Exception. (str "Unknown node at path element " t1))))))


(defn grammar-path
  "Given a grammar and a full grammar path (including the rule key),
  prune the grammar to include only the alt values specified in the
  grammar path."
  [grammar path]
  (assert (grammar-node grammar path) (str "no node at path: " path))
  {(first path) (grammar-path* (get grammar (first path)) (rest path))})

(defn grammar-node-string
  "Print the EBNF grammar for a single grammar node (i.e. call on the
  result of grammar-node)"
  [node]
  (instaparse.print/combinators->str node))

(defn grammar-path-string
  "Use grammar-path to prune the grammar and then print the
  EBNF grammar for that pruned grammar."
  [grammar path]
  (let [pruned-grammar (grammar-path grammar path)
        rule (first path)]
    (instaparse.print/rule->str rule (get pruned-grammar rule))))


(comment

(def p (load-parser :html))

(def html "<html><head><link rel=\"stylesheet\" href=\"/static/normalize.css\"><link rel=\"stylesheet\" href=\"/static/rend.css\"></head><body>x<div style=\"background: red\"></div></body></html>")

(def w (instacheck/filter-alts (parse-weights p html)))

;; List used rules sorted by rule name
(doseq [[k v] (sort-by (comp str key) w)]
  (println (str (grammar-path-string (:grammar p) k)
                "  (* {:weight " v "} *)")))

;; List used rules sorted by rule weight (usage frequency)
(doseq [[k v] (sort-by val w)]
  (println (str (grammar-path-string (:grammar p) k)
                "  (* {:weight " v "} *)")))


)


(comment

(time (def hp (load-parser :html-parse)))
(time (def cp (load-parser :css-parse)))

(def text (slurp "test/html/example.com-20190422.html"))
(def text (slurp "test/html/mozilla.com-20190506.html"))
;; Parses HTML, issues with CSS
(def text (slurp "test/html/apple.com-20190422.html"))

(def html    (extract-html text))
(def css-map (extract-css-map text "test/html"))

(time (def hw (instacheck/filter-alts (parse-weights hp html))))
(time (def cw (instacheck/filter-alts (parse-weights cp (vals css-map)))))

)
