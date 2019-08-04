(ns wend.core
  (:require [clojure.java.io :as io]
            [clojure.string :as S]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [hickory.core]
            [hickory.render]
            [hickory.select :as s]
            [flatland.ordered.map :refer [ordered-map]] 
            [mend.parse]
            [instacheck.core :as icore]))

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
                                 (if (string? v) (S/trim v) v)])))
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
      (S/replace #"[^\x00-\x7f]" "")
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
      (S/replace #"[\xc9]" "&#x00c9;")))

(defn cleanup-css
  [css]
  (-> css
      ;; Remove unicode characters
      (S/replace #"[^\x00-\x7f]" "")
      ;; Remove non-unix newlines
      (S/replace #"[\r]" "\n")
      ;; remove vendor prefixes
      (S/replace #"([^A-Za-z0-9])(?:-webkit-|-moz-|-ms-)" "$1")
      ;; Remove apple specific CSS property
      (S/replace #"x-content: *\"[^\"]*\"" "")
      ;; Some at-rule syntax require semicolons before closing curly
      (S/replace #"(@font-face *[{][^}]*[^;])[}]" "$1;}")
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
        style (S/join
                ";\n"
                (filter (complement empty?)
                        (map #(S/replace % #";\s*$" "")
                             styles)))]
    (str "* {\n" style "\n}")))

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
                       (S/join "\n    " styles)
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
(print (S/join "\n\n" (vals css-map)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

(time (def hp (load-parser :html-parse)))
(time (def cp (load-parser :css-parse)))

(def text (slurp "test/html/example.com-20190422.html"))
(def text (slurp "test/html/mozilla.com-20190506.html"))
;; Parses HTML, issues with CSS
(def text (slurp "test/html/apple.com-20190422.html"))

(def html    (extract-html text))
(def css-map (extract-css-map text "test/html"))

(time (def hw (instacheck.grammar/filter-trek-weighted (parse-weights hp html))))
(time (def cw (instacheck.grammar/filter-trek-weighted (parse-weights cp (vals css-map)))))

)

