(ns mend.cli
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]

            [instacheck.cli :as instacheck-cli]
            [instacheck.core :as instacheck]
            [mend.gen :as mend.gen]
            [mend.parse :as mend.parse]))

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
        "Mode: html or css."
        :validate [#(get #{"html" "css"} %) "Must be 'html' or 'css'"]]
       [nil "--clj-output CLJ-OUTPUT"
        "Write Clojure code to path."]
       [nil "--grammar-output GRAMMAR-OUTPUT"
        "Write EDN grammar tree to path."]
       [nil "--namespace NAMESPACE"
        "Name of namespace to generate"]
       [nil "--function FUNCTION"
        "Emit a function named FUNCTION which returns a generator rather than a defn for every generator"]])))

(defn ebnf->clj [opts]
  (let [mode (keyword (:mode opts))

        ctx (merge {:weights-res (atom {})}
                   (select-keys opts [:namespace
                                      :weights :function]))


        ;; The following each take 4-6 seconds
        _ (println "Loading" mode "parser grammar")
        parse-grammar (instacheck/parser->grammar
                            (mend.parse/load-parser mode :parse))
        _ (println "Loading" mode "generator grammar")
        gen-grammar (instacheck/parser->grammar
                          (mend.parse/load-parser mode :gen))
        _ (println "Applying generator grammar updates")
        mangled-gen-grammar    (mend.gen/grammar-update gen-grammar mode)
        _ (println "Converting generator grammar to clojure generators")
        ns-str (instacheck/grammar->ns
                 ctx mangled-gen-grammar "[rend.misc-generators :as rgen]")]

    (when-let [gfile (:grammar-output opts)]
      (println "Saving parser grammar to" gfile)
      (spit gfile (with-out-str (pprint parse-grammar))))

    (when-let [wfile (:weights-output opts)]
      (println "Saving generator weights to" wfile)
      (instacheck/save-weights wfile @(:weights-res ctx)))

    ns-str))

(defn -main
  "This takes about 20-60 seconds to run"
  [& args]
  (let [opts (:options (opt-errors (parse-opts args cli-options)))]
    (when (not (:mode opts))
      (println "--mode MODE (html or css) is required")
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

