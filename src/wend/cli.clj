(ns wend.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]

            [mend.ebnf :as ebnf]
            [instaparse.core :as insta]))

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
  (pr-err "[OPTS] <EBNF-FILE> <FILE>...")
  (System/exit 2))

(defn -main
  [& args]
  (let [opts (opt-errors (parse-opts args
                                     cli-options :in-order true))
        {:keys [verbose weights-output]} (:options opts)
        [ebnf & files] (:arguments opts)
        [vprn vprintln vpprint] (if verbose
                                  [prn println pprint]
                                  [list list list])
        parser (insta/parser ebnf)
        all-paths (atom [])
        _ (println "Calculating full weight list")
        ;; Get the full weight set by calling the def generator but
        ;; throwing away the result. The weights are in the context
        ;; atom
        ctx {:weights-res (atom {})}
        _ (ebnf/grammar->generator-defs-source ctx (ebnf/trim-parser parser))
        ;; Zero out weights
        base-weights (into {} (for [[k v] @(:weights-res ctx)] [k 0]))]
    (vprn :ebnf ebnf :files files :weights-output weights-output)
    (vprintln "All base weights:")
    (vpprint base-weights)
    (vprintln "EBNF Grammar:")
    (vprn parser)
    (doseq [file files]
      (println "\nProcessing" file)
      (let [parsed (insta/parse parser (slurp file) :unhide :all)]
        (swap! all-paths concat (-> parsed meta :path-log))
        (vprintln (str "Parsed result from" file ":"))
        (vpprint parsed)
        (vprintln (str "Weights from " file ":"))
        (vpprint (frequencies (-> parsed meta :path-log)))))
    (let [file-weights (frequencies @all-paths)
          merged-weights (merge base-weights file-weights)]
      (vprintln "Combined file weights:")
      (vpprint file-weights)
      (vprintln "Final merged weights:")
      (vpprint merged-weights)
      (when weights-output 
        (println "Writing weights to" weights-output)
        (spit weights-output (with-out-str (pprint merged-weights)))))))
