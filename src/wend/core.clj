(ns wend.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [hickory.core]
            [hickory.render]
            [instaparse.core]
            [instaparse.print]))

(def EBNF-PATHS ["data/html5.ebnf"
                 "data/css3.ebnf"])

(def GENERIC-EBNF-PATHS ["data/html5-generic.ebnf"
                         "data/html5.ebnf"
                         "data/css3-generic.ebnf"
                         "data/css3.ebnf"])

(def BASE-MANGLES
  {:attr-val-style :css-assignments
   :attr-val-media :css-media-list})

(def GENERIC-MANGLES
  {:char-data :char-data-generic
   :comment :comment-generic
   :url :url-generic
   :stylesheet-placeholder :stylesheet
   :css-media-list-placeholder :css-media-list})

(defn mangle-parser
  [parser mangles]
  (reduce (fn [p [k v]] (assoc-in p [:grammar k]
                                  {:tag :nt, :keyword v
                                   :red {:reduction-type :hiccup, :key k}}))
          parser mangles))

(defn load-parser* [ebnf-paths]
  (let [ebnf (string/join "\n" (map slurp ebnf-paths))
        base-parser (instaparse.core/parser ebnf)
        parser (mangle-parser base-parser BASE-MANGLES)]
    parser))

(defn load-parser []
  (load-parser* EBNF-PATHS))

(defn load-generic-parser []
  (let [parser (load-parser* GENERIC-EBNF-PATHS)]
    (mangle-parser parser GENERIC-MANGLES)))


(defn prune-meta-with-property
  "Takes hickory and prunes meta tags with property attributes"
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (vector? n)
              (vec (remove #(and (= :meta (-> % :tag))
                                 (-> % :attrs :property)) n))
              n))
    h))

#_(defn prune-nav-role-attribute
  "Takes hickory and prunes role attributes from nav tags."
  [h]
  (clojure.walk/prewalk
    (fn [n] (if (= :nav (-> n :tag))
              ;; i.e. (dissoc-in n [:attrs :role])
              (assoc n :attrs (dissoc (:attrs n) :role))
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

(defn normalize-html
  "Pass HTML through hickory and back to get normalize the HTML into
  a more standard a consistent form." 
  [html]
  (-> html
      hickory.core/parse
      hickory.core/as-hickory
      prune-meta-with-property
      ;;prune-nav-role-attribute
      cleanup-ws-in-attrs
      hickory.render/hickory-to-html))

;; hickory-data (hick/as-hickory (hick/parse include-data))

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

(defn parse-weights [parser text]
  (let [hparsed (parser text)]
    (if (instance? instaparse.gll.Failure hparsed)
      ;;(throw (ex-info "Parser failure" {:failure hparsed})))
      (throw (Exception. (pr-str hparsed))))
    (frequencies (-> hparsed meta :path-log))))

(defn save-weights [path weights]
  (spit path (with-out-str (pprint (into (sorted-map) weights)))))

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

(def p (load-parser))

(def html "<html><head><link rel=\"stylesheet\" href=\"../static/normalize.css\"><link rel=\"stylesheet\" href=\"../static/rend.css\"></head><body>x<div style=\"background: red\"></div></body></html>")

(def w (filter-alts (parse-weights p html)))

(doseq [k (keys w)] (println (grammar-path-string (:grammar p) k)))

)


(comment

(time (def p (load-generic-parser)))

(def w (filter-alts (parse-weights p (slurp "test/html/example.com-20190422.html"))))

(def w (filter-alts (parse-weights p (normalize-html (slurp "test/html/apple.com-20190422.html")))))

)
