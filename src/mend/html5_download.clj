(ns mend.html5-download
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [org.httpkit.client :as http]
            [hickory.core :as hick]
            [hickory.select :as s])
  (:import [java.net URI]
           [javax.net.ssl SSLEngine SSLParameters SNIHostName]))

;; https://developer.mozilla.org/en-US/docs/Web/HTML/Reference
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
;; https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
;; https://dev.w3.org/html5/html-author/
;; https://en.wikipedia.org/wiki/HTML_attribute#Standard_attributes

(def HTML-BASE-URI "https://developer.mozilla.org")
(def HTML-ELEM-PATH "/en-US/docs/Web/HTML/Element")
(def HTML-ATTR-PATH "/en-US/docs/Web/HTML/Attributes")

(defn sni-configure
  [^SSLEngine ssl-engine ^URI uri]
  (let [^SSLParameters ssl-params (.getSSLParameters ssl-engine)]
    (.setServerNames ssl-params [(SNIHostName. (.getHost uri))])
    (.setSSLParameters ssl-engine ssl-params)))

;; Downloads the HTML element reference and returns a map of
;; element/tag name to relative URL
(defn get-html-elements []
  (let [client (http/make-client {:ssl-configurer sni-configure})
        ;; Get the MDN HTML element reference page
        resp @(http/get (str HTML-BASE-URI HTML-ELEM-PATH) {:client client})
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
             (-> a :attrs)]))))

(defn deref-content [h]
  (if (map? h) (recur (-> h :content first)) h))

(defn extract-name [h]
  (string/replace (deref-content h) #"^<|>$" ""))

(defn extract-text [h]
  (if (string? h)
    h
    (apply str (map extract-text (:content h)))))

;; Downloads the HTML attribute reference and returns a map of
;; attribute name to relative URL
(defn get-html-attributes []
  (let [client (http/make-client {:ssl-configurer sni-configure})
        ;; Get the MDN HTML element reference page
        resp @(http/get (str HTML-BASE-URI HTML-ATTR-PATH) {:client client})
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
               ;; Extracted fields should match drop count below
               (s/and (s/tag :td)
                      (s/or (s/nth-child 1)
                            (s/nth-child 2)
                            (s/nth-child 3))))
             all-data)]
    (loop [kw-map {}
           data as]
      (let [[a elems desc] data]
        (assert (or (nil? a)
                    (= :code (-> a :content first :tag))
                    (str "Invalid attr td: " a)))
        (if (nil? a)
          kw-map
          (let [aname (deref-content a)
                tags (s/select
                       (s/descendant (s/tag :a))
                       elems)
                title (-> a :content last :attrs :title)
                desc (extract-text desc)
                desc (if title (str title "\n" desc) desc)]
            ;;(prn :aname aname :desc (extract-text desc))
            (recur
              (assoc kw-map aname {:elems (vec (map extract-name tags))
                                   :desc desc})
              ;; This should match the number of extracted fields above
              (drop 3 data))))))))

(defn -main [& args]
  (let [elements-file "data/html5-elements.json"
        attributes-file "data/html5-attributes.json"
        _ (println "Downloading HTML5 elements")
        elements (get-html-elements)
        _ (println (str "  Writing " (count elements)
                        " elements to " elements-file))
        _ (spit elements-file (json/write-str elements))
        _ (println "Downloading HTML5 attributes")
        attributes (get-html-attributes)
        _ (println (str "  Writing " (count attributes)
                        " attributes to " attributes-file))
        _ (spit attributes-file (json/write-str attributes))]
    ))

