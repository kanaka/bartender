(ns mend.cli
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]

            [instacheck.cli :as instacheck-cli]
            [instacheck.grammar :as instacheck-grammar]
            [mend.core :as mend]
            [wend.core :as wend]))

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
  (vec
    (concat
      instacheck-cli/general-cli-options
      [[nil "--mode MODE"
        "Mode: html5 or css3."
        :validate [#(get #{"html5" "css3"} %) "Must be 'html5' or 'css3'"]]
       [nil "--clj-output CLJ-OUTPUT"
        "Write Clojure code to path."]
       [nil "--grammar-output GRAMMAR-OUTPUT"
        "Write EDN grammar tree to path."]
       [nil "--namespace NAMESPACE"
        "Name of namespace to generate"]
       [nil "--function FUNCTION"
        "Emit a function named FUNCTION which returns a generator rather than a defn for every generator"]])))

(defn ebnf->clj [opts]
  (let [grammar-update (condp = (:mode opts)
                         "html5" mend/html5-grammar-update-fn
                         "css3"  mend/css3-grammar-update-fn)
                         ;;"css3"  css3-grammar-updates)
        grammar-type (condp = (:mode opts)
                       "html5" :html-gen
                       "css3"  :css-gen)

        ctx (merge {:weights-res (atom {})}
                   (select-keys opts [:namespace
                                      :weights :function]))


        ;; The following each take 4-6 seconds
        _ (println "Loading" grammar-type "grammar")
        raw-grammar (instacheck-grammar/parser->grammar
                      (wend/load-parser grammar-type))
        _ (println "Apply grammar updates")
        grammar (grammar-update raw-grammar)
        _ (println "Converting grammar to clojure generators")
        ns-str (mend/grammar->ns ctx grammar)]

    (when-let [gfile (:grammar-output opts)]
      (println "Saving grammar to" gfile)
      (spit gfile (with-out-str (pprint grammar))))

    (when-let [wfile (:weights-output opts)]
      (println "Saving weights to" wfile)
      (wend/save-weights wfile @(:weights-res ctx)))

    ns-str))

(defn -main
  "This takes about 10-30 seconds to run"
  [& args]
  (let [opts (:options (opt-errors (parse-opts args cli-options)))]
    (when (not (:mode opts))
      (println "--mode MODE (html5 or css3) is required")
      (System/exit 2))
    (when (not (:clj-output opts))
      (println "--clj-output CLJ-OUTPUT is required")
      (System/exit 2))
    (when (not (:namespace opts))
      (println "--namespace NAMESPACE required")
      (System/exit 2))

    (println "Saving Clojure code to" (:clj-output opts))
    (io/make-parents (:clj-output opts))
    (spit (:clj-output opts) (ebnf->clj opts))))

