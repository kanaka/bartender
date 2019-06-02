(ns wend.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]

            [instacheck.core :as instacheck]
            [wend.core :as wend]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of wend

(def cli-options
  [[nil "--debug" "Add debug comments to generated code"]
   [nil "--verbose" "Verbose output during execution"]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights out the file."]])

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
        {:keys [weights-output]} (:options opts)
        [& files] (:arguments opts)
        _ (pr-err "Loading HTML parser")
        html-parser (wend/load-parser :html-parse)
        _ (pr-err "Loading CSS parser")
        css-parser (wend/load-parser :css-parse)]
    (loop [weights {}
           files files]
      (let [[file & rest-files] files
            _ (pr-err (str "Processing: '" file "'"))
            fdir (.getParent (io/file file))
            text (slurp file)
            html (wend/extract-html text)
            css-map (wend/extract-css-map text fdir)
            _ (pr-err "  - parsing HTML")
            html-weights (wend/parse-weights html-parser html)
            ahtml-weights (instacheck/filter-alts html-weights)
            _ (pr-err (str "  - HTML weights all: " (count html-weights)
                            ", alts: " (count ahtml-weights)))
            _ (pr-err "  - parsing CSS")
            css-weights (wend/parse-weights css-parser (vals css-map))
            acss-weights (instacheck/filter-alts css-weights)
            _ (pr-err (str "  - CSS weights all: " (count css-weights)
                            ", alts: " (count acss-weights)))
            new-weights (merge-with + weights ahtml-weights acss-weights)]
        (if (seq rest-files)
          (recur new-weights rest-files)
          (when weights-output
            (pr-err (str "Saving merged alt weights to: '" weights-output "'"))
            (wend/save-weights weights-output new-weights)))))))
