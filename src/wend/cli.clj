(ns wend.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]

            [instacheck.core :as instacheck]
            [mend.parse]
            [wend.core :as wend]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn filter-parse-data
  "Filter out the zero weights from the :parts :wtrek and :full-wtrek
  data."
  [data]
  {:parts (for [p (:parts data)]
            (assoc p :wtrek
                   (into {} (filter #(not= 0 (val %))
                                    (:wtrek p)))))
   :full-wtrek (into {} (filter #(not= 0 (val %))
                                (:full-wtrek data)))})
(defn parse-files
  [html-parser css-parser files]
  (loop [data {:parts []
               :full {}}
         files files]
    (let [[file & rest-files] files
          _ (pr-err (str "Processing: '" file "'"))
          fdir (.getParent (io/file file))
          text (slurp file)
          html (wend/extract-html text)
          css-map (wend/extract-css-map text fdir)
          _ (pr-err "  - parsing HTML")
          html-data-all (instacheck/parse-wtreks
                          html-parser [[html file]])
          html-data (filter-parse-data html-data-all)
          _ (pr-err (str "  - HTML weights: "
                         (count (:full-wtrek html-data)) "/"
                         (count (:full-wtrek html-data-all))))
          _ (pr-err "  - parsing CSS")
          css-data-all (instacheck/parse-wtreks
                         css-parser (for [[k v] css-map] [v k]))
          css-data (filter-parse-data css-data-all)
          _ (pr-err (str "  - CSS weights: "
                         (count (:full-wtrek css-data)) "/"
                         (count (:full-wtrek css-data-all))))
          new-data {:parts (vec (concat (:parts data)
                                        (:parts html-data)
                                        (:parts css-data)))
                    :full-wtrek (merge-with +
                                            (:full-wtrek data)
                                            (:full-wtrek html-data)
                                            (:full-wtrek css-data))}]
      (if (seq rest-files)
        (recur new-data rest-files)
        new-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of wend

(def cli-options
  [[nil "--debug" "Add debug comments to generated code"]
   [nil "--verbose" "Verbose output during execution"]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights to WEIGHTS-OUTPUT"]
   [nil "--parse-output PARSE-OUTPUT" "Write resulting parse data to PARSE-OUTPUT"]])

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(defn usage []
  (pr-err "[OPTS] <FILE>...")
  (System/exit 2))

(defn -main
  [& args]
  (let [opts (opt-errors (parse-opts args
                                     cli-options :in-order true))
        {:keys [parse-output weights-output]} (:options opts)
        [& files] (:arguments opts)
        _ (pr-err "Loading HTML parser")
        html-parser (mend.parse/load-parser-from-grammar :html)
        _ (pr-err "Loading CSS parser")
        css-parser (mend.parse/load-parser-from-grammar :css)
        parse-data (parse-files html-parser css-parser files)]
    (pr-err (str "Combined and filtered weights: "
                 (count (:full-wtrek parse-data))))
    (when parse-output
      (pr-err (str "Saving parse data to: '" parse-output "'"))
      (spit parse-output parse-data))
    (when weights-output
      (pr-err (str "Saving merged weights to: '" weights-output "'"))
      (instacheck/save-weights weights-output (:full-wtrek parse-data)))))
