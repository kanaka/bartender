(ns mend.css-download
  (:require [mend.util :as util]
            [org.httpkit.client :as http]
            [hickory.core :as hick]
            [hickory.select :as s]
            [clojure.java.io :refer [as-file make-parents]]))

;; https://developer.mozilla.org/en-US/docs/Web/CSS/Reference
;;   - look at formal syntax section to generate properties
;; https://developer.mozilla.org/en-US/docs/Template:csssyntax
;;   - csssyntax macro that pulls syntax onto MDN page
;; https://github.com/mdn/data/tree/master/css
;;   - directory of CSS JSON data files (for csssyntax macro)
;; https://raw.githubusercontent.com/mdn/data/master/css/syntaxes.json
;;   - raw syntax data
;; http://www.blooberry.com/indexdot/css/propindex/all.htm
;; https://www.w3.org/Style/CSS/all-properties.en.html

(def CSS-BASE-URI "https://developer.mozilla.org")
(def CSS-REF-PATH "/en-US/docs/Web/CSS/Reference")

(defn get-css-keywords []
  (let [;; Get the MDN CSS reference page
        resp @(http/get (str CSS-BASE-URI CSS-REF-PATH))
        ;; Parse it
        all-data (hick/as-hickory (hick/parse (:body resp)))
        ;; Pull out the keyword index
        as (s/select
             (s/descendant
               (s/and (s/class "index")
                      s/first-child)
               (s/tag :ul)
               (s/tag :li)
               (s/tag :a))
             all-data)
        kw-map (into {}
                     (for [a as]
                       [(-> a :content first :content first)
                        (-> a :attrs :href)]))]
    kw-map))

;; Download the formal syntax for a single property
;; Returns a hickory format of the syntax HTML
(defn get-formal-syntax-hickory [path]
  (let [resp @(http/get (str CSS-BASE-URI path))
        all-data (hick/as-hickory (hick/parse (:body resp)))
        sb (s/select (s/class "syntaxbox")
                     all-data)]
    (:content (first sb))))

;; Takes the result of get-formal-syntax-hickory and outputs a full
;; formal syntax text string for the property. Strips the "where"
;; lines and prepends the property starting node name.
(defn formal-syntax [prop hickory-data]
  (let [pruned (filter #(not (re-find #"^ *where *$" %))
                       (util/inner-text hickory-data))]
    (str (apply str "<'" prop "'> = " pruned) "\n")))

(defn filter-css-properties [kws]
  (let [ffn (fn [[name path]]
              (and
                (re-find #"^[a-z-]+$" name)
                (not (re-find #"#" path))))]
    (into {} (filter ffn kws))))

(defn get-all-css-properties [outdir]
  (let [all-props (filter-css-properties (get-css-keywords))]
    (make-parents (str outdir "/file"))
    (doseq [[prop path] (sort all-props)]
      (let [full-path (str outdir "/" prop ".pvs")]
        (if (.exists (as-file full-path))
          (println "Already exists:" prop)
          (do
            (println "Retrieving:" prop)
            (let [pdata (get-formal-syntax-hickory path)
                  text (formal-syntax prop pdata)]
              (spit (str outdir "/" prop ".pvs") text))))))))

(comment

(def css-kws (get-css-keywords))
(count css-kws)
(count (filter-css-properties css-kws))

(def ts-hick (get-formal-syntax-hickory "/en-US/docs/Web/CSS/text-shadow"))
(println (formal-syntax "text-shadow" ts-hick))

(get-all-css-properties "data/css-syntax/")

)
