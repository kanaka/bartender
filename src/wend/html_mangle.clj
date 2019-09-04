(ns wend.html-mangle
  (:require [clojure.java.io :as io]
            [clojure.string :as S]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [flatland.ordered.map :refer [ordered-map]]
            [mend.parse]
            [instacheck.core :as icore]))

(def PRUNE-TAGS
  #{"style"
    "script"
    "svg"
    "font"})

(def PRUNE-TAGS-BY-ATTRIBUTE
  ;; [tag attribute]
  #{["meta" "property"]})

(def PRUNE-TAGS-BY-ATTRIBUTE-VALUE
  ;; [tag attribute value]
  #{["link" "rel" "stylesheet"]})

(def PRUNE-ATTRIBUTES
  #{"style"
    "align"
    "x-ms-format-detection"
    "data-viewport-emitter-state"
    "windowdimensionstracker"})

(def PRUNE-TAG-ATTRIBUTES
  ;; tag -> [attribute ...]
  {"input" ["autocapitalize" "autocorrect"]
   "link" ["as" "color"]
   "meta" ["value"]
   "iframe" ["frameborder" "scrolling"]
   "div" ["type"]
   "span" ["for" "fahrenheit"]
   "td" ["width"]
   "table" ["cellspacing" "cellpadding" "frame" "width"]

   ;; TODO: these are HTML 5+ and shouldn't be removed when parsing
   ;; that.
   "video" ["playsinline" "webkit-playsinline"]
   })

(def REWRITE-TAG-ATTRIBUTE-VALUES
  ;; {[tag attribute value] new-value}
  {["link" "rel" "styleSheet"] "stylesheet"
   ["select" "required" "required"] "true"
   ["table" "border" "0"] ""})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags and Attrs Method

(def TnA-parser (mend.parse/load-parser :tNa :parse))

(defn TnA-parse
  [text]
  (icore/parse TnA-parser text))

;; ---

(defn pr-noval-attr [[aname {:keys [asp nsp vsp avals] :as sp-avals}]]
  (str (first asp) aname (first nsp)))

(defn pr-all-attrs [[aname {:keys [asp nsp vsp avals] :as sp-avals}]]
  (S/join
    ""
    (map #(str %1 aname %2 "=" %3 "\"" %4 "\"") asp nsp vsp avals)))

(defn pr-first-attr [[aname {:keys [asp nsp vsp avals] :as sp-avals}]]
  (str (first asp) aname (first nsp)
       "=" (first vsp) "\"" (first avals) "\""))

(defn pr-merged-attr [[aname {:keys [asp nsp vsp avals]} :as attr]]
  (let [avals (filter #(not (re-seq #"^\s*$" %)) avals)]
    (condp = aname
      "style" (str (first asp) aname (first nsp)
                   "=" (first vsp) "\"" (S/join "; " avals) "\"")
      "class" (str (first asp) aname (first nsp)
                   "=" (first vsp) "\"" (S/join " "  avals) "\"")
      (pr-all-attrs attr))))

(defn- trim-surrounding-spaces
  [text]
    (-> text (S/replace #"^\s*" "") (S/replace #"\s*$" ""))
  )

(defn- attr-map
  [attrs-elem]
  (loop [attrs (ordered-map)
         attr-elems (rest attrs-elem)]
    (if (seq attr-elems)
      (let [[attr & attr-elems] attr-elems
            [_ asp [_ aname] nsp [_ vsp aval :as has-aval?]] attr
            ;; Empty string is different than no value and should be
            ;; retained
            aval (if aval
                   (trim-surrounding-spaces aval)
                   (if has-aval?
                     ""
                     nil))]
        (recur (-> attrs
                   (update-in [aname :asp]   (fnil conj []) asp)
                   (update-in [aname :nsp]   (fnil conj []) nsp)
                   (update-in [aname :vsp]   (fnil conj []) vsp)
                   (update-in [aname :avals] (fnil conj []) aval))
               attr-elems))
      attrs)))

(defn- elem-to-text
  [elem {:keys [cur-tag
                dupe-attr-mode
                trim-spaces?
                prune-tags
                prune-tags-by-attr
                prune-tags-by-attr-val
                prune-attrs
                prune-tag-attrs
                rewrite-tag-attr-vals]}]
  (let [tag (nth elem 2)
        all-attrs (attr-map (nth elem 3))
        ta-set (set (map vector
                         (repeat tag)
                         (keys all-attrs)))
        tav-set (set (map vector
                          (repeat tag)
                          (keys all-attrs)
                          (map (comp first :avals) (vals all-attrs))))
        attrs (reduce
                (fn [attrs [aname {:keys [asp nsp vsp avals] :as sp-avals}]]
                  (if (or
                        ;; Prune by :prune-attrs
                        (contains? (set prune-attrs) aname)
                        ;; Prune by :prune-tag-attrs
                        (contains? (set (get prune-tag-attrs tag)) aname))
                    attrs
                    (let [rewr-val (get rewrite-tag-attr-vals
                                        [tag aname (first avals)])
                          sp-avals {:asp (if trim-spaces?
                                           (repeat (count asp) " ")
                                           asp)
                                    :nsp (if trim-spaces?
                                           (repeat (count nsp) "")
                                           nsp)
                                    :vsp (if trim-spaces?
                                           (repeat (count vsp) "")
                                           vsp)
                                    :avals (if rewr-val
                                             [rewr-val]
                                             avals)}]
                      (assoc attrs aname sp-avals))))
                (ordered-map)
                all-attrs)]
    (if (or
          ;; Prune by :prune-tags
          (contains? (set prune-tags) tag)
          ;; TODO: these miss end-elem in the start-elem case
          ;; Prune by :prune-tags-by-attr
          (seq (set/intersection prune-tags-by-attr ta-set))
          ;; Prune by :prune-tags-by-attr-val
          (seq (set/intersection prune-tags-by-attr-val tav-set)))
      (if trim-spaces?
        []
        ;; Just the final spaces to keep subsequent tag indent unchanged
        (reduce #(if (re-seq #"\S+" %2) [] (conj %1 %2)) [] (drop 7 elem)))
      (concat
        ;; '<' and tag name
        (take 2 (drop 1 elem))
        ;; attributes
        (mapcat (fn [[_ {:keys [avals]} :as attr]]
                  (if (= [nil] avals)
                    (pr-noval-attr attr)
                    (condp = dupe-attr-mode
                      :first (pr-first-attr attr)
                      :merge (pr-merged-attr attr)
                      (pr-all-attrs attr))))
                attrs)
        (if (and trim-spaces?
                 (not (#{"style" "script"} tag)))
          ;; trim starting and ending spaces
          (map trim-surrounding-spaces (drop 5 elem))
          ;; spaces, '>', spaces
          (drop 4 elem))))))

(defn- elem-depths
  [elem cur-depth]
  (let [kind (if (string? elem) :string (first (second elem)))]
    (cond
      (and (= kind :start-elem)
           (#{"meta" "link"} (nth (second elem) 2)))
      [cur-depth cur-depth]

      (= kind :start-elem)
      [cur-depth (inc cur-depth)]

      (= kind :end-elem)
      [(dec cur-depth) (dec cur-depth)]

      :default
      [cur-depth cur-depth])))

(defn TnA->html
  "Opts:
    :dupe-attr-mode         - how to handle duplicate attributes name
    :trim-spaces?           - remove extra leading/trailing spaces
    :reindent?              - reindent everything (implies :trim-spaces?)
    :prune-wrap-ahem?       - remove wrap-ahem span tags
    :prune-tags             - tags to omit by [tag]
    :prune-tags-by-attr     - tags to omit by [tag, attr]
    :prune-tags-by-attr-val - tags to omit by [tag, attr, value]
    :prune-attrs            - tag attributes to omit by [attr]
    :prune-tag-attrs        - tag attributes to omit by [tag, attr]
    :rewrite-tag-attr-vals  - change attribute value by [tag, attr, val]
  "
  [TnA & [{:keys [trim-spaces? reindent?
                  prune-wrap-ahem? prune-tags] :as opts}]]
  (assert (= :html (first TnA))
          "Not a valid parsed HTML grammar")
  (let [trim-spaces? (or reindent? trim-spaces?)
        opts (assoc opts :trim-spaces? trim-spaces?)
        maybe-trim (fn [s] (if trim-spaces?
                             (trim-surrounding-spaces s)
                             s))
        start-spaces (maybe-trim (second TnA))]
    (loop [res []
           depth 0
           TnA (drop 2 TnA)]
      (let [[elem & next-TnA] TnA
            [cur-depth new-depth] (elem-depths elem depth)
            res (if reindent?
                  (conj res (apply str "\n" (repeat cur-depth "  ")))
                  res)]
        (if (not elem)
          (if reindent?
            (S/join "" (drop 1 res))
            (str start-spaces (S/join "" res)))
          (recur
            (cond
              (string? elem)
              (conj res (maybe-trim elem))

              (= :content (first (second elem)))
              (apply conj res (map maybe-trim (rest (second elem))))

              (and prune-wrap-ahem?
                   (= :ahem-elem (first (second elem))))
              res

              (= :end-elem (first (second elem)))
              (if (contains? (set prune-tags) (nth (second elem) 2))
                ;; Drop end tag for prune-tags. Matching start tag is
                ;; handled in elem-to-text.
                (apply conj res (map maybe-trim (drop 4 (second elem))))
                (apply conj res (map maybe-trim (rest (second elem)))))

              :else
              (apply conj res (elem-to-text (second elem) opts)))
            new-depth
            next-TnA))))))

(defn extract-html
  "Returns a cleaned up and normalized HTML text string. Primarily
  this involves removing or rewriting unsupported tags and
  attributes."
  [html]
  (let [html-no-unicode (S/replace html #"[^\x00-\x7f]" "")
        TnA (TnA-parse html-no-unicode)]
    (TnA->html
      TnA
      {:dupe-attr-mode         :first
       :trim-spaces?           true
       :prune-wrap-ahem?       true
       :prune-tags             PRUNE-TAGS
       :prune-tags-by-attr     PRUNE-TAGS-BY-ATTRIBUTE
       :prune-tags-by-attr-val PRUNE-TAGS-BY-ATTRIBUTE-VALUE
       :prune-attrs            PRUNE-ATTRIBUTES
       :prune-tag-attrs        PRUNE-TAG-ATTRIBUTES
       :rewrite-tag-attr-vals  REWRITE-TAG-ATTRIBUTE-VALUES})))

(defn- extract-inline-css-from-TnA
  "Internal: takes a TnA parse and returns text of all inline styles.
  Used by extract-inline-css and extract-css-map."
  [TnA]
  (let [attrs (reduce
                (fn [r elem]
                  (if (and (vector? elem)
                           (not (get #{:end-elem :content}
                                     (first (second elem))))
                           (= :attrs (-> elem second (nth 3) first)))
                    (apply conj r (-> elem second (nth 3) rest))
                    r))
                []
                TnA)
        sattrs (filter #(= [:attr-name "style"] (nth % 2)) attrs)
        styles (filter (complement empty?)
                       (map #(S/replace (last (nth % 4)) #";\s*$" "")
                            sattrs))]
    (str (S/join ";\n" styles))))

(defn cleanup-css
  [css-text]
  (-> css-text
      ;; Remove unicode characters
      (S/replace #"[^\x00-\x7f]" "")
      ;; Remove non-unix newlines
      (S/replace #"[\r]" "\n")
      ;; remove vendor prefixes
      (S/replace #"([^A-Za-z0-9])(?:-webkit-|-moz-|-ms-)" "$1")
      ;; Remove apple specific CSS property
      (S/replace #"x-content: *\"[^\"]*\"" "")
      ;; Some at-rule syntax require semicolons before closing curly
      (S/replace #"(@font-face *[{][^}]*[^;])[}]" "$1;}")))


(defn extract-inline-css
  "Return text of all inline styles. Specifically it extracts the
  content from style attributes and merges it into a single CSS block
  that is surrounded by a wildcard selector."
  [html]
  (cleanup-css
    (extract-inline-css-from-TnA (TnA-parse html))))

(defn extract-css-map
  "Return a map of CSS texts with the following keys:
  - :inline-style  -> all inline styles in
                      a wildcard selector (i.e. '* { STYLES }')
  - :inline-sheet-X -> inline stylesheets by indexed keyword
  - \"sheet-href\"  -> loaded stylesheets by path/URL

  The returned styles can be combined into a single stylesheet like this:
      (clojure.string/join \"\n\" (apply concat CSS-MAP))"
  [html & [get-file]]
  (let [get-file (or get-file slurp)
        TnA (TnA-parse html)
        ;; inline via style attribute
        inline-styles (str
                        "* {\n"
                        (cleanup-css
                          (extract-inline-css-from-TnA TnA))
                        "\n}")
        ;; inline via style tag
        style-elems (filter #(and (vector? %)
                                  (= :style-elem (-> % second first)))
                            TnA)
        inline-sheets (map #(-> % second (nth 6)) style-elems)
        ;; external via link tag
        link-elems (filter #(and (vector? %)
                                 (= :start-elem (-> % second first))
                                 (= "link" (-> % second (nth 2))))
                           TnA)
        link-attrs (map #(-> % second (nth 3) attr-map) link-elems)
        sheet-attrs (filter #(-> % (get "rel") :avals
                                 first S/lower-case (= "stylesheet"))
                            link-attrs)
        sheet-hrefs (map #(-> % (get "href") :avals first) sheet-attrs)
        loaded-sheets (map #(str "/* from: " % " */\n"
                                 (cleanup-css
                                   (get-file %)))
                           sheet-hrefs)]
    (merge {:inline-styles inline-styles}
           (zipmap (map (comp keyword str)
                        (repeat "inline-sheet-")
                        (range))
                   inline-sheets)
           (zipmap (map str sheet-hrefs)
                   loaded-sheets))))


(comment

(def hp (mend.parse/load-parser-from-grammar :html :parse))
(def cp (mend.parse/load-parser-from-grammar :css :parse))

(def text (slurp "test/html/basics.html"))
(def text (slurp "test/html/example.com-20190422.html"))
(def text (slurp "test/html/smedberg-20190701-2.html"))
;; HTML: success, CSS: @-webkit-keyframes
(def text (slurp "test/html/apple.com-20190422-2.html"))
;; HTML: '[' in attr val, CSS: url with no quoting
(def text (slurp "test/html/github.com-20190422.html"))
;; HTML: success, CSS: success (18 seconds)
(def text (slurp "test/html/mozilla.com-20190506.html"))
;; HTML: itemscope, CSS: -webkit...rgba(
(def text (slurp "test/html/google.com-20190422.html"))
;; HTML: success, CSS: url with no quoting
(def text (slurp "test/html/cnn.com-20190422.html"))

(def text (slurp "test/html/ssllabs.com-20190816.html"))

(time (def html    (extract-html text)))
(time (def css-map (extract-css-map text #(slurp (io/file "test/html" %)))))

(time (def hw (icore/parse-wtrek hp html)))
(time (def cw (icore/parse-wtreks cp (zipmap (vals css-map) (keys css-map)))))

(pprint (filter #(> (second %) 0) (:wtrek hw)))
(pprint (filter #(> (second %) 0) (:full-wtrek cw)))

(print (wend.cli/parser-wtrek->ebnf hp (:wtrek hw)))
(print (wend.cli/parser-wtrek->ebnf cp (:full-wtrek cw)))

)
