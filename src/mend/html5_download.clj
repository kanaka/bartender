(ns mend.html-download
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [org.httpkit.client :as http]
            [hickory.core :as hick]
            [hickory.select :as s]
            ))

;; https://developer.mozilla.org/en-US/docs/Web/HTML/Reference
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
;; https://dev.w3.org/html5/html-author/
;; https://en.wikipedia.org/wiki/HTML_attribute#Standard_attributes

(def HTML-BASE-URI "https://developer.mozilla.org")
(def HTML-ELEM-PATH "/en-US/docs/Web/HTML/Element")
(def HTML-ATTR-PATH "/en-US/docs/Web/HTML/Attributes")

;; Downloads the HTML element reference and returns a map of
;; element/tag name to relative URL
(defn get-html-elements []
  (let [;; Get the MDN HTML element reference page
        resp @(http/get (str HTML-BASE-URI HTML-ELEM-PATH))
        ;; Parse it
        all-data (hick/as-hickory (hick/parse (:body resp)))
        ;; Pull out the keyword index
        as (s/select
             (s/descendant
               ;(s/and (s/class "standard-table")
               ;       s/first-child)
               (s/id "wikiArticle")
               (s/class "standard-table")
               (s/tag :tr)
               (s/and (s/tag :td)
                      s/first-child)
               (s/tag :a))
             all-data)]
    (into {}
          (for [a as]
            [(string/replace (-> a :content first :content first)
                             #"^<|>$" "")
             (-> a :attrs :href)]))))

;; Downloads the HTML attribute reference and returns a map of
;; attribute name to relative URL
(defn get-html-attributes []
  (let [;; Get the MDN HTML element reference page
        resp @(http/get (str HTML-BASE-URI HTML-ATTR-PATH))
        ;; Parse it
        all-data (hick/as-hickory (hick/parse (:body resp)))
        ;; Pull out the keyword index
        as (s/select
             (s/descendant
               ;(s/and (s/class "standard-table")
               ;       s/first-child)
               (s/id "wikiArticle")
               (s/class "standard-table")
               (s/tag :tr)
               (s/and (s/tag :td)
                      (s/or (s/nth-child 1)
                            (s/nth-child 2))))
             all-data)
        ename (fn [h]
                (string/replace
                  (let [h1 (-> h :content first)]
                    (if (string? h1)
                      h1
                      (-> h1 :content first)))
                  #"^<|>$" ""))]
    (loop [kw-map {}
           data as]
      (let [[a elems] data]
        (assert (or (nil? a)
                    (= :code (-> a :content first :tag))
                    (str "Invalid attr td: " a)))
        (if (nil? a)
          kw-map
          (let [aname (-> a :content first :content first)
                tags (s/select
                       (s/descendant (s/tag :a))
                       elems)]
            (recur
              (assoc kw-map aname (vec (map ename tags)))
              (drop 2 data))))))))

(comment

(spit "data/html5-elements.json" (json/write-str (get-html-elements)))
(spit "data/html5-attributes.json" (json/write-str (get-html-attributes)))

)

