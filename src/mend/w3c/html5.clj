(ns mend.w3c.html5
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]

            [instaparse.core :as instaparse]

            [hickory.core :as hick]
            [hickory.select :as s]))

;; https://developer.mozilla.org/en-US/docs/Web/HTML/Reference
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
;; - XML ebnf:
;;   http://bottlecaps.de/rr/ui
;;   https://www.w3.org/TR/2000/REC-xml-20001006
;; - HTML ebnf:
;;   https://github.com/lisp/de.setf.xml/blob/master/bnf/html-grammar.bnf
;; http://w3c.github.io/html/syntax.html#void-elements


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing W3C standards data

(defn deref-content
  "Recursively pull text content out of hickory data
  (remove internal tags)"
  [h]
  (cond
    (map? h)    (recur (-> h :content))
    (vector? h) (apply str (apply concat (for [c h] (deref-content c))))
    :else       h))

(defn id->name [id]
  (string/replace id #"[^\s]+/" ""))

(defn id->terminal [id]
  (string/replace id #"/" "__"))

(defn item-names
  "Extract items names of the form <{name}>..."
  [s]
  (map second (re-seq #"<[{]([\S]+)[}]>" s)))

(defn header-mangle
  "Translate W3C section table header into map keyword."
  [h]
  (-> h
      deref-content
      (string/replace #"[^A-Za-z0-9_.]" "")
      string/lower-case
      keyword))

(defn field-mangle
  "Translate the W3C section table (hickory) data into map values. The
  specific translation depends on the field name/keyword."
  [k v]
  (condp = k
    ;; Elements (section/attributes.include)
    :element    (item-names (deref-content v))
    :categories (string/split (deref-content v) #"\s*;\s*")
    :attributes (item-names (deref-content v))

    ;; Attributes (section/attributes.include)
    :attribute  (string/replace (deref-content v) #"[<{}>]*" "")
    :elements   (string/split
                  (string/replace (deref-content v) #"[<{}>]*" "")
                  #"\s*[\n;]\s*")
    ;; Extract literals (surrounded by <code>) and
    ;; metadata/descriptive values (surrounded by <a>).
    :value      (let [literals (s/select (s/descendant
                                           (s/tag :code)) v)
                      metas (s/select (s/descendant
                                        (s/and
                                          (s/tag :a)
                                          (s/not (s/has-descendant
                                                   (s/tag :code))))) v)]
                  {:raw (deref-content v)
                   :literals (set (map deref-content literals))
                   :metas (set (map deref-content metas))})

    ;; Input Semantics (section/semantics-forms.include)
    ;; no special handling

    ;; Autofill Semantics (section/semantics-forms.include)
    :token (string/replace (string/trim (deref-content v)) #"[<{}>\"]*" "")

    ;; Default just content without code marks
    (string/replace (string/trim (deref-content v)) #"[<{}>]*" "")))

(defn parse-w3c-include
  "Given the raw W3C section HTML for elements, attributes or
  semantics, return a list of maps with the parsed data.

  Parsing elements.include returns a list of maps each of this form:
  {:element \"a\",
   :description \"Hyperlink\",
   :categories [\"flow\" \"phrasing*\" \"interactive\"],
   :parents \"phrasing\",
   :children \"transparent*\",
   :attributes (\"links/href\" \"links/target\" \"links/download\" ...),
   :interface \"{{HTMLAnchorElement}}\"}

  Parsing attributes.include returns a list of maps each of this form:
  {:attribute \"abbr\",
   :elements [\"th\"],
   :description \"Alternative label to use for the header cell ...\",
   :value {:raw \"<a>Text</a>\"
           :literals #{},
           :metas #{\"Text\"}}}

  Parsing semantics-forms.include returns a list of maps each of this form:
  {:keyword \"text\",
   :state \"Text\",
   :datatype \"Text with no line breaks\",
   :controltype \"A text field or combo box\"}
  "
  [include-data table-idx]
  (let [table-idx (or table-idx 0)
        hickory-data (hick/as-hickory (hick/parse include-data))
        ;; Pull the tables out of the hickory data
        tables (s/select (s/descendant (s/tag :table)) hickory-data)
        ;; Select the table-idx'th table
        table (nth tables table-idx)
        ;; Pull out the rows from the table
        chunks (s/select
                 (s/descendant
                   (s/tag :tr))
                 table)
        ;; Pull out the header and cell data
        rows (for [c chunks] (s/select
                               (s/descendant
                                 (s/or (s/tag :th)
                                       (s/tag :td)))
                               c))
        ;; Mangle the headers and zipmap with the cell data
        headers (map header-mangle (first rows))
        zipped (for [v (rest rows)]
                    (zipmap headers v))
        ;; Mangle the field/cell data
        mangled (for [row zipped]
                  (with-meta
                    (into {} (map (fn [[k v]]
                                    [k (field-mangle k v)]) row))
                    row))
        kind (first headers)
        full (condp = kind
               ;; Split multiple items in :element field (currently
               ;; just elements h1-h6)
               :element (for [item mangled
                              iname (get item :element)]
                          (assoc item :element iname))
               ;; Split multiple elements in :elements field and
               ;; prefix :attribute with elements (if not already
               ;; prefixed)
               :attribute (for [item mangled
                                raw-ename (get item :elements)]
                            (let [ename (if (= "HTML elements" raw-ename)
                                          "global"
                                          raw-ename)
                                  ;; Rewrite with single element
                                  item (assoc (dissoc item :elements)
                                              :element ename)
                                  raw-aname (name (:attribute item))
                                  ;; Ignore prefix as it may be
                                  ;; a category (i.e. "formelements")
                                  ;; rather than an single element
                                  [aname _] (reverse
                                              (string/split raw-aname #"/"))]
                              (assoc item :attribute (str ename "/" aname))))
               mangled)]
    full))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EBNF generation

;; Void tags do not have a closing tag.
;; http://w3c.github.io/html/syntax.html#void-elements
(defn void-element [elem] (= "empty" (:children elem)))

;; Boolean only tags can appear without an assignment value
(defn boolean-attribute [attr] (contains? (-> attr :value :metas) "Boolean attribute"))

;; Special elements appear once and in specific order
(defn special-element [elem] (#{"html" "head" "title" "body"} (:element elem)))

(defn ebnf-tag-rhs
  [element]
  (let [tag-name (:element element)]
    (if (void-element element)
      [;; void tag (no end tag)
       (str "'<" tag-name "' "
            "(<rS> " tag-name "-attribute)* "
            "'>'")
;; For now don't emit self-closing void elements
;;     ;; void tag (self-closing mark is allowed)
;;     (str "'<" tag-name "' "
;;          "(<rS> " tag-name "-attribute)* "
;;          "'/>'")
       ]
      [;; normal tag (i.e. <tag ...> ... </tag>)
       (str "'<" tag-name "' "
            "(<rS> " tag-name "-attribute)* "
            "'>' (element | content)* "
            "'</" tag-name ">'")])))

(defn ebnf-element
  [element]
  (let [lhs "element "
        pre "        "]
    (str
      lhs "= "
      (string/join
        (str "\n" pre "| ")
        (mapcat ebnf-tag-rhs element)))))

(defn ebnf-tag-attrs
  [tag-name attrs & append]
  (let [lhs (str tag-name "-attribute ")
        pre (apply str (repeat (count lhs) " "))
        a-ids (fn [pred? as]
                (sort-by id->name (set (map :attribute (filter pred? as)))))
        bool-attrs (a-ids boolean-attribute attrs)
        other-attrs (a-ids #(not (boolean-attribute %)) attrs)]
    (str
      lhs "= "
      (string/join
        (str "\n" pre "| ")
        (concat
          (for [a-id bool-attrs]
            (str "'" (id->name a-id) "' opt-boolean "))
          (for [a-id other-attrs]
            (str "'" (id->name a-id)
                 "=\"' attr-val-" (id->terminal a-id) " '\"'"))
          append)))))

(defn ebnf-attr-val-expand
  [attr pre]
  (let [{:keys [literals metas raw]} (:value attr)]
    (concat
      (for [value (sort literals)]
        (str "'" value "'"))

      (for [value (sort metas)]
        (condp = value
          "Valid integer"                (str "integer")
          "Valid non-negative integer"   (str "non-negative-integer")
          "Valid floating-point number"  (str "floating-point-number")
          "Boolean attribute"            (str "'true'\n" pre "| 'false'")
          "Text"                         (str "attribute-data")
          "ID"                           (str "name")
          "Encoding label"               (str "encoding-label")
          "Valid MIME type"                                 (str "mime-type")
          "valid MIME types with no parameters"             (str "mime-type")
          "Set of space-separated tokens"                   (str "name ( <rS> name )*")
          "Unordered set of unique space-separated tokens"  (str "name ( <rS> name )*")
          "Ordered set of unique space-separated tokens"    (str "name ( <rS> name )*")
          "Valid browsing context name or keyword"          (str "name")

          "Valid non-empty URL potentially surrounded by spaces" (str "url")
          "Valid URL potentially surrounded by spaces"           (str "url")
          (str "'STUB " value "'")))

      (condp = raw
        "Varies*"                                       [(str "attribute-data")]
        "Valid BCP 47 language tag"                     [(str "lang")]
        "Valid BCP 47 language tag or the empty string" [(str "lang")]
        [])

      (condp = (:description attr)
        "Horizontal dimension"  [(str "non-negative-integer ( length-unit | '%')")]
        "Vertical dimension"    [(str "non-negative-integer ( length-unit | '%')")]
        []))))

(defn ebnf-attr-vals
  [attr]
  (let [lhs (str "attr-val-" (id->terminal (:attribute attr)) " ")
        pre (apply str (repeat (count lhs) " "))
        rhs-seq (ebnf-attr-val-expand attr pre)]
    (str
      ;; Always have an empty value
      lhs "= ''"
      ;;lhs "= '' | '\"\"'"
      ;; If there are other values then output separator
      (when (> (count rhs-seq) 0)
        (str "\n" pre "| "))
      ;; Output values
      (string/join
        (str "\n" pre "| ")
        rhs-seq))))

(defn ebnf-elements-attributes
  "Takes an element list, an attribute list, and a form semantics list
  and returns an EBNF grammar for HTML5 elements, attributes (global
  and non-global), and input form types."
  [elements attributes input-attrs autofill-attrs]
  (let [elems (filter (complement special-element) elements)
        input-types (set (map :keyword input-attrs))
        autofill-values (set (map :token autofill-attrs))

        non-bool-attrs (filter #(not (boolean-attribute %)) attributes)
        unique-attrs (map (comp first val)
                          (group-by :attribute non-bool-attrs))
        enriched-attrs (map #(cond
                               (= "input/type" (:attribute %))
                               (assoc-in % [:value :literals] input-types)

                               (= "input/autocomplete" (:attribute %))
                               (assoc-in
                                 (assoc-in % [:value :literals] autofill-values)
                                 [:value :metas] #{})

                               :else
                               %)
                            unique-attrs)]
    (str
      (ebnf-element (sort-by :element elems))
      "\n\n"
      (ebnf-tag-attrs "global" (filter #(= "global" (:element %)) attributes)
                      "custom-data-attribute"
                      "aria-attribute"
                      "role-attribute"
                      "event-attribute")
      "\n\n"
      (string/join
        "\n"
        (for [e (sort-by :element elems)
              :let [elem (:element e)
                    attrs (filter #(= elem (:element %)) attributes)]]
          (ebnf-tag-attrs elem attrs
                          "global-attribute")))
      "\n\n"
      (string/join
        "\n"
        (for [a (sort-by :attribute enriched-attrs)]
          (ebnf-attr-vals a))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(def cli-options
  [[nil "--ebnf-output EBNF-OUTPUT"
    "Write intermediate EBNF to file"
    :default "./data/html5.ebnf"]
   [nil "--elements-include ELEMENTS-INCLUDE"
    "Path to W3C include section for HTML5 elements"
    :default "w3c_html/sections/elements.include"]
   [nil "--attributes-include ATTRIBUTES-INCLUDE"
    "Path to W3C include section for HTML5 attributes"
    :default "w3c_html/sections/attributes.include"]
   [nil "--form-semantics-include FORM-SEMANTICS-INCLUDE"
    "Path to W3C include section for HTML5 form semantics"
    :default "w3c_html/sections/semantics-forms.include"]
   [nil "--ebnf-prefix EBNF-PREFIX"
    "Path to prefix file to include in EBNF output"
    :default "./data/html5-prefix.ebnf"]
   [nil "--ebnf-base EBNF-BASE"
    "Path to base grammar file to include in EBNF output"
    :default "./data/html5-base.ebnf"]
   [nil "--ebnf-common EBNF-COMMON"
    "Path to common rules to include in EBNF output"
    :default "./data/common.ebnf"]])

(defn ebnf-combined-str
  "Take opts with prefix, base, elements and attributes data paths
  and return an EBNF grammar as a string that represents the full
  HTML5 EBNF syntax."
  [opts]
  (let [elements (slurp (:elements-include opts))
        attributes (slurp (:attributes-include opts))
        form-semantics (slurp (:form-semantics-include opts))]
    (string/join
      "\n\n"
      ["(* This was generated by mend.w3c.html5 *)"
       (slurp (:ebnf-prefix opts))
       (ebnf-elements-attributes
         (parse-w3c-include elements 0)
         (parse-w3c-include attributes 0)
         (parse-w3c-include form-semantics 1)
         (parse-w3c-include form-semantics 7))
       (slurp (:ebnf-base opts))
       (slurp (:ebnf-common opts))])))


(defn -main
  "Generate a HTML 5 EBNF grammar based on specification data from the
  W3C.

  This takes about 7 seconds to run"
  [& args]
  (let [opts (:options (opt-errors (parse-opts args cli-options)))
        input-opts (dissoc opts :ebnf-output)
        _ (println "Generating HTML5 EBNF based on:" (vals input-opts))
        html5-ebnf-str (ebnf-combined-str input-opts)]

    (println "Saving HTML5 EBNF to" (:ebnf-output opts))
    (spit (:ebnf-output opts) html5-ebnf-str)

    (println "Verifying HTML5 EBNF grammar")
    (instaparse/parser html5-ebnf-str)))

