(ns rend.generator
  (:require [clojure.string :as string]
            [clojure.test.check.generators :as gen]
            [wend.html-mangle :as html-mangle]

            [instacheck.util :as util]
            [rend.css3-generators :as css3-gen]
            [rend.html5-generators :as html5-gen]))

(defn escape-text
  [text]
  (string/escape text {\" "\\\""}))

(defn get-html-generator [& [weights]]
  (let [css-gen-map (css3-gen/css3-generators {} weights)
        ;; Pull out css-assignments and do double-quote escaping
        ;; of strings in it so that the result can be inserted into
        ;; a style="" tag (which is already surrounded by double
        ;; quotes)
        new-css-gen-map (->> css-gen-map
                             :css-assignments-test
                             (gen/fmap util/flatten-text)
                             (gen/fmap escape-text)
                             (assoc css-gen-map :css-assignments-test))


	html-gen-map (html5-gen/html5-generators new-css-gen-map weights)
        gen-html (gen/fmap
                   #(-> %
                        util/flatten-text
                        html-mangle/TnA-parse
                        (html-mangle/TnA->html {:dupe-attr-mode :merge}))
                   (:html-test html-gen-map))]
    gen-html))

(defn html-start-value
  "Unfortunately even size 1 still allows optional 'foo?' items to
  either appear or so generate a bunch and take the smallest.  This is
  not foolproof but size 0 returns nil unfortunately."
  [html-generator]
  (->> (gen/sample-seq html-generator 1)
       (take 50)
       (sort-by count)
       first))
