(ns wend.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [hickory.core]
            [hickory.render]
            [hickory.select :as s]
            [instaparse.core]
            [instaparse.print]
            [instaparse.failure]))

(def EBNF-PATHS
  {:html5-gen      ["data/html5.ebnf"]
   :html5-parse    ["resources/html5-generic.ebnf"
                    "data/html5.ebnf"]
   :css3-gen       ["data/css3.ebnf"]
   :css3-parse     ["data/css3.ebnf"
                    "resources/css3-generic.ebnf"]})

(def GRAMMAR-MANGLES
  {:html5-gen      {}
   :html5-parse    {:char-data :char-data-generic
                    :comment :comment-generic
                    :url :url-generic}
   :css3-gen       {}
   :css3-parse     {:nonprop-group-rule-body :stylesheet
                    :prop-group-rule-body :css-ruleset
                    :nonprop-declaration-list :css-assignments}})

(def START-RULES
  {:html5-gen      :html
   :html5-parse    :html-generic
   :css3-gen       :css-assignments
   :css3-parse     :stylesheet})

(defn mangle-parser
  [parser mangles]
  (reduce (fn [p [k v]] (assoc-in p [:grammar k]
                                  {:tag :nt, :keyword v
                                   :red {:reduction-type :hiccup, :key k}}))
          parser mangles))

(defn load-parser* [paths mangles]
  (let [ebnf (string/join "\n" (map slurp paths))
        base-parser (instaparse.core/parser ebnf)
        parser (mangle-parser base-parser mangles)]
    parser))

(defn load-parser [kind]
  (let [parser (load-parser* (get EBNF-PATHS kind) (get GRAMMAR-MANGLES kind))]
    (assoc parser :start-production (get START-RULES kind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def PRUNE-TAGS
  #{:style
    :script})

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
      hickory.core/parse
      hickory.core/as-hickory
      prune-tags
      prune-attributes
      prune-tag-attributes
      cleanup-ws-in-attrs
      hickory.render/hickory-to-html))

(defn cleanup-css
  [css]
  (string/replace
    (string/replace
      (string/replace
        css
        ;; Remove non-unix newlines
        #"[\r]" "\n")
      ;; remove vendor prefixes
      #"([^A-Za-z0-9])(?:-webkit-|-moz-|-ms-)" "$1")
    ;; Remove apple specific CSS property
    #"x-content: *\"[^\"]*\"" ""))

;;        ;; remove vendor prefixes (from at-rules)
;;        #"@(-webkit-|-moz-|-ms-)" "@")
;;      ;; Remove vendor prefixes (-webkit-, -moz-, -ms-) from some properties
;;      #"(-webkit-|-moz-|-ms-)(transform|text-size-adjust|box-sizing|font-feature-settings)\b" "$2"

(defn extract-css-map
  "Return a map of CSS texts with the following keys:
  - :loaded-sheets -> list of stylesheets loaded by path/URL
  - :inline-sheets -> list of stylesheets inline in the HTML
  - :inline-styles -> list with single text with all inline styles in
                      a wildcard selector (i.e. '* { STYLES }')

  The returned styles can be combined into a single stylesheet like this:
      (clojure.string/join \"\n\" (apply concat CSS-MAP))"
  [html & [base-path]]
  (let [h (-> html
              hickory.core/parse
              hickory.core/as-hickory)
        ;; Extract inline tag specific styles
        styles (map #(->> % :attrs :style)
                    (s/select (s/child (s/attr :style)) h))
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
        inline-styles (when (seq styles)
                        (str
                          "* {\n    "
                          (string/join "\n    " styles)
                          "\n}"))
        css-map {:loaded-sheets (map cleanup-css loaded-sheets)
                 :inline-sheets (map cleanup-css inline-sheets)
                 :inline-styles (list (cleanup-css inline-styles))}]
    css-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare parse-weights filter-alts)

;;(def reducible-regex #"^element$|^[\S-]*-attribute$|^css-assignment$")
(def reducible-regex #"^\[:element :alt [0-9]+\]$|^\[[\S-]*-attribute :alt [0-9]+\]$|^\[css-assignment :alt [0-9]+\]$")
(defn reducer-half [w] (int (/ w 2)))

(defn reduce-weights [weights parser text reducer]
  (let [wparsed (parse-weights parser text)
        wreduced (for [[p w] (filter-alts wparsed)
                       :when (and (get weights p)
                                  (re-seq reducible-regex (str p)))]
                   [p (reducer (get weights p))])]
    (into {} wreduced)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar weight functions

;; TODO: these generic functions should move to instacheck

(defn prune-zero-weights
  "Remove weight paths with weight value of zero."
  [weights]
  (into {} (filter (comp pos? val) weights)))

(defn weight-leaves
  "Return the leaf nodes of the weights data (i.e. prune intermediate
  weights paths)."
  [weights]
  (let [keyfn (fn [[k v]] (string/join "-" k))
        sorted-kvs (sort-by keyfn weights) ]
    (reduce (fn [res [k' v' :as kv']]
             (let [[k v :as kv] (last res)]
               (if (= k (take (count k) k'))
                 (conj (vec (butlast res)) kv')
                 (conj res kv'))))
            []
            sorted-kvs)))

(defn filter-alts
  "Remove paths for weights that are not alternations. Only
  alternations (gen/frequency) are currently affected by the weights
  so remove everything else."
  [weights]
  (into {} (filter #(-> % key reverse second (= :alt)) weights)))

(declare checked-parse) ;; TODO: use from instacheck

(defn parse-weights [parser texts]
  (let [texts (if (string? texts) [texts] texts)
        parsed (map #(checked-parse parser %) texts)]
    (apply merge-with +
           (map #(frequencies (-> % meta :path-log)) parsed))))

(defn save-weights [path weights]
  (spit path (with-out-str (pprint (into (sorted-map) weights)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better instaparse errors

;; TODO: these generic functions should move to instacheck

(defn elide-line
  [text cursor max-length cont-text]
  (let [span (/ max-length 2)
        start (max 0            (- cursor span))
        end   (min (count text) (+ cursor span))]
    (str
      (when (> cursor span) cont-text)
      (subs text start end)
      (when (> (count text) max-length) cont-text))))

(defn concise-fail-str
  [failure text]
  (let [err-str (with-out-str (instaparse.failure/pprint-failure failure))
        column (:column failure)
        [pos text-line mark-line & reasons] (string/split err-str #"\n")
        text-line (string/replace text-line #"\t" " ")
        text-line (elide-line text-line column 200 "...")
        mark-line (elide-line mark-line column 200 "   ")

        reasons (string/join "\n" reasons)]
    (string/join "\n" [pos text-line mark-line reasons])))

(defn checked-parse [parser text]
  (let [res (parser text)]
    (if (instance? instaparse.gll.Failure res)
      (throw (Exception. (concise-fail-str res text)))
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar/weight path functions

;; TODO: these generic functions should move to instacheck

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

(def html "<html><head><link rel=\"stylesheet\" href=\"../static/normalize.css\"><link rel=\"stylesheet\" href=\"../static/rend.css\"></head><body>x<div style=\"background: red\"></div></body></html>")

(def w (filter-alts (parse-weights p html)))

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

(time (def p (load-generic-parser)))

(def w (filter-alts (parse-weights p (slurp "test/html/example.com-20190422.html"))))

(def w (filter-alts (parse-weights p (normalize-html (slurp "test/html/apple.com-20190422.html")))))

)
