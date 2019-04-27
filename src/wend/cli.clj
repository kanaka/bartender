(ns wend.cli
  (:require [clojure.string :as string]

            [clojure.tools.cli :refer [parse-opts]]

            [instaparse.core :as insta]
            [mend.util :as util]

            ;; Not actually used here, but convenient for testing
            [clojure.pprint :refer [pprint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

;;(defn load-grammar
;;  [ebnf]
;;  (let [parser (insta/parser ebnf)]
;;    (with-meta
;;      (util/remove-key (:grammar parser) :red)
;;      {:start (:start-production parser)})))

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
        [ebnf & files] (:arguments opts)
        parser (insta/parser ebnf)]
    (prn :ebnf ebnf :files files)
    (println "Parser:")
    (prn parser)
    (doseq [file files]
      (let [parsed (insta/parse parser (slurp file) :unhide :all)]
      ;(let [parsed (insta/parse parser (slurp file) :trace true)]
        (println "Parsed:")
        (prn parsed)
        (println "Parsed grammar:")
        (prn parsed)
        (prn :meta-parsed (meta parsed))
        ))))
