(ns mend.ebnf
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [alandipert.kahn :as kahn]
            [clojure.java.io :refer [as-file]]
            [mend.util :as util]
            [clojure.walk :as walk]
            [clojure.edn :as edn]

            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]]

            ;; Not actually used here, but convenient for testing
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-grammar
  [ebnf]
  (let [parser (insta/parser ebnf)]
    (with-meta
      (util/remove-key (:grammar parser) :red)
      {:start (:start-production parser)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create test.check generators for sub-trees of an
;; instaparse grammar rule

(declare gen-ROUTE)

(defn- gen-cat
  "Each value must occur in order."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE (update-in ctx [:path] conj 0)
                 (-> tree :parsers first) indent)
      (str pre "(gen/tuple\n"
           (string/join
             "\n"
             (for [[t idx] (zipmap (-> tree :parsers) (range))
                   :let [ctx (update-in ctx [:path] conj idx)]]
               (gen-ROUTE ctx t (+ 1 indent))))
           ")"))))


(defn- gen-alt
  "One of the values must occur."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))
        weights-res (:weights-res ctx)]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE (update-in ctx [:path] conj 0)
                 (-> tree :parsers first) indent)
      (str pre "(gen/frequency [\n"
           (string/join
             "\n"
	     (for [[t idx] (zipmap (-> tree :parsers) (range))
                   :let [ctx (update-in ctx [:path] conj idx)
                         pw (get (:weights ctx) (:path ctx))
                         weight (if pw pw 100)
                         wcomment (when pw
                                    "    ;; *** adjusted by config ***")]]
               (do
                 (when weights-res
                   (swap! weights-res assoc (:path ctx) weight))
                 (str pre "  [" weight wcomment "\n"
                      (gen-ROUTE ctx t (+ 2 indent)) "]"))))
           "])"))))

(defn- gen-regexp
  "Value must match regexp. For common space value \\s* and \\s+
  generate zero and 1 space respectively."
  [ctx tree indent]
  (let [re (:regexp tree)
        pre (apply str (repeat indent "  "))]
    (cond
      (= (str #"\s*") (str re))
      (str pre "(gen/return \"\")")

      (= (str #"\s+") (str re))
      (str pre "(gen/tuple (gen/return \" \"))")

      :else
      (str pre "(chuck/string-from-regex " (pr-str re) ")"))))

;; TODO: :hide option
(defn- gen-string
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return " (pr-str (:string tree)) ")")))

(defn- gen-star
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) ")")))

(defn- gen-plus
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/such-that not-empty (gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "))")))

(defn- gen-opt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/one-of [\n"
         pre "  (gen/return \"\")\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "])")))

(defn- gen-epsilon
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return \"\")")))

;; TODO: mutual recursion?
;; TODO: :hide option
(defn- gen-nt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))
        kw (:keyword tree)]
    (str pre (if (= (:cur-nt ctx) kw)
               "inner"
               (str "gen-" (name kw))))))

;;;;;;

(def tag-to-gen
  {:cat     gen-cat
   :alt     gen-alt
   :regexp  gen-regexp
   :string  gen-string
   :star    gen-star
   :plus    gen-plus
   :opt     gen-opt
   :epsilon gen-epsilon
   :nt      gen-nt})

(defn- gen-ROUTE
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))
        tag (:tag tree)
        f (get tag-to-gen (:tag tree))
        ;; Update path
        ctx (update-in ctx [:path] conj tag)]
    (assert f (str "No generator found for " tag))
    (str (if (:debug ctx)
           (str pre ";; path: " (:path ctx) "\n")
           "")
         (f ctx tree indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create generators for full instaparse grammar including
;; immediately recursive grammars.


(defn- prune-rule-recursion
  "Prune a grammar rule of the recursive parts. Identify the smallest
  optional branches of the rule which are recursive and prune/remove
  them."
  [k tree]
  (let [parent? (fn [t] (util/tree-matches
                          #(= % {:tag :nt :keyword k}) t))]
    (walk/postwalk
      (fn [node]
        (if (and (map? node) (parent? node))
          ;; Prune/rewrite the matches sub-trees
          (condp = (:tag node)
            :alt  {:tag :alt
                   :parsers (filter #(not (parent? %))
                                    (:parsers node))}
            :star {:tag :epsilon}
            :opt  {:tag :epsilon}
            node)
          node))
      tree)))

(defn- prune-grammar-recursion
  "The test.check gen-recursive generator takes a recursive generator
  and a non-recursive generator as options. For directly recursive
  grammar rules, a version of the rule without the recursion (smallest
  optional part of the rule that recurses is removed) is used for the
  non-recursive part of gen-recursive and the full recursive rule is
  used for the recursive generator to recursive-gen."
  [grammar]
  (into {} (for [[k rule] grammar]
             [k (prune-rule-recursion k rule)])))

;;;;;;

(defn- gen-rule-body
  "Takes a rule name, rule grammar and indent level and returns the
  text of a generator for the rule body."
  [ctx k v indent]
  (let [pre (apply str (repeat indent "  "))
        ctx (assoc ctx :cur-nt k :path [k])]
    (if (util/tree-matches #(= k %) v)
      (str pre "(gen/recursive-gen\n"
           pre "  (fn [inner]\n"
           (gen-ROUTE ctx v (+ 2 indent)) ")\n"
           (gen-ROUTE ctx (prune-rule-recursion k v) (+ 1 indent)) ")")
      (str (gen-ROUTE ctx v indent)))))

(defn- check-and-order-rules
  "Takes an instaparse grammar and returns a sequence of the rule
  names in the order that they can be defined (reverse dependency
  order). If the grammar contains mutually recursive (non-direct)
  rules it will throw an indicating which rules are cyclic."
  [grammar]
  (let [deps (util/tree-deps grammar)
        pruned-deps (kahn/remove-direct-recursive deps)
        _ (assert (empty? (kahn/mutually-recursive pruned-deps))
                  (str "Mutually recursive generators unsupported:"
                       (kahn/mutually-recursive pruned-deps)))
        ordered-rules (reverse (kahn/kahn-sort-throws pruned-deps))]
    ordered-rules))

;;;;;;

(defn grammar->generator-defs
  "Takes an grammar (loaded using load-grammar) and returns the text
  of top-level defines (defs) for all the rules. If the :start is
  specified in the ctx then this the name of the rule to use as the
  starting rule of the grammar. If :start is not specified then the
  first rule in the grammar file is used as the starting rule. Only
  the start rule flattens the generated values into a final string."
  [ctx grammar]
  (let [ordered-rules (check-and-order-rules grammar)
       start (or (:start ctx) (:start (meta grammar)))]
    (string/join
      "\n\n"
      (for [k ordered-rules
            :let [v (get grammar k)]]
        (if (= start k)
          (str "(def gen-" (name k) "\n"
               "  (gen/fmap util/flatten-text\n"
               (gen-rule-body ctx k v 2) "))")
          (str "(def gen-" (name k) "\n"
               (gen-rule-body ctx k v 1) ")"))))))

(defn grammar->generator-let
  "Takes an grammar (loaded using load-grammar) and returns the text
  of a Clojure let block that defines all the rules and returns the
  start rule. The returned text/code can then be read and eval'd to
  return a generator. If the :start is specified in the ctx then this
  the name of the rule to use as the starting rule of the grammar. If
  :start is not specified then the first rule in the grammar file is
  used as the starting rule."
  [ctx grammar]
  (let [ordered-rules (check-and-order-rules grammar)
        start (or (:start ctx) (:start (meta grammar)))]
    (str "(let ["
         (string/join
           "\n\n      "
           (for [k ordered-rules
                 :let [v (get grammar k)]]
             (str "gen-" (name k) "\n"
                  (gen-rule-body ctx k v 3))))
         "]\n"
         "  (gen/fmap util/flatten-text\n"
         "    gen-" (name start)  "))")))

(defn- grammar->generator
  [ctx grammar]
  (binding [*ns* (create-ns 'mend.ebnf)]
    (eval (read-string (grammar->generator-let ctx grammar)))))

(defn ebnf-gen
  "Takes an path to an EBNF grammar file and return a test.check
  generator. If the start is specified then this the name of the rule
  to use as the starting rule of the grmmar. If start is not specified
  then the first rule in the grammar file is used as the starting
  rule."
  ([ebnf] (ebnf-gen {} ebnf))
  ([ctx ebnf] (if (map? ebnf)
                (grammar->generator ctx ebnf)
                (grammar->generator ctx (load-grammar ebnf)))))


(comment
  (println (grammar->generator-let {} (load-grammar (slurp "test/recur3.ebnf"))))

  (def ebnf-generator (ebnf-gen (slurp "test/recur3.ebnf")))
  (pprint (gen/sample ebnf-generator 10))
)

;;;;;;

(defn- prefix [ctx]
  (str
"(ns " (:namespace ctx) "
  (:require [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]
            [mend.util :as util]))

"))

(defn grammar->ns
  [ctx grammar]
  (assert (:namespace ctx) ":namespace required in ctx")
  (str (prefix ctx) (grammar->generator-defs ctx grammar)))

(comment
  (def ebnf-grammar (load-grammar (slurp "test/recur1.ebnf")))
  (spit "joel/gen.clj" (grammar->ns {:namespace "joel.gen"}
                                    ebnf-grammar))
)

(defn run-tests
  [ctx raw-cmd samples]
  (loop [results []
         samples samples]
    (let [samp (first samples)
          samples (next samples)
          temp-file (java.io.File/createTempFile "ebnf" ".data")
          temp-writer (io/writer temp-file)
          tpath (.getCanonicalPath ^java.io.File temp-file)
          cmd (if (seq (keep #(re-find #"%" %) raw-cmd))
                (map #(string/replace % #"%" tpath) raw-cmd)
                (conj raw-cmd tpath))
          res (try
                (println "Running:" (string/join " " cmd))
                (.write temp-writer samp)
                (.flush temp-writer)
                (apply sh cmd)
                (finally
                  (.delete temp-file)))
          results (conj results (:exit res))]
      (println "Result:"
              (if (= 0 (:exit res))
                "Pass"
                (str "Fail (exit code " (:exit res) ")")))
      (if samples
        (recur results samples)
        (if (every? zero? results)
          true
          false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of ebnf

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(def cli-options
  [[nil "--weights WEIGHTS"
    "An EDN data file containing frequency weights (map of ctx path to weight value) to use (default weight is 100)."
    :default {}
    :parse-fn #(edn/read-string (slurp %))]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights out the file."]
   [nil "--start START" "Starting grammar rule"]])

(def cmd-options
  {"clj-let" [[nil "--debug" "Add debug comments to generated code"]]
   "clj-ns" [[nil "--debug" "Add debug comments to generated code"]
             [nil "--namespace NAMESPACE" "Name of namespace to generate"]]
   "gen" [[nil "--samples SAMPLES" "Number of samples to generate"
           :default 10]]
   "test" [[nil "--iterations ITERATIONS" "Test iterations"
            :default 10]]})

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(defn usage []
  (pr-err "ebnf [GLOBAL-OPTS] clj-let <EBNF-FILE> [LET-OPTS]")
  (pr-err "ebnf [GLOBAL-OPTS] clj-ns  <EBNF-FILE> [NS-OPTS]")
  (pr-err "ebnf [GLOBAL-OPTS] gen     <EBNF-FILE> [GEN-OPTS]")
  (pr-err "ebnf [GLOBAL-OPTS] test    <EBNF-FILE> [TEST-OPTS] -- <CMD>")
  (System/exit 2))

(defn -main
  [& args]
  (let [top-opts (opt-errors (parse-opts args
                                         cli-options :in-order true))
        [cmd ebnf & cmd-args] (:arguments top-opts)
        _ (when (not (and ebnf
                          cmd
                          (#{"clj-let" "clj-ns" "gen" "test"} cmd)))
            (usage))
        cmd-opts (opt-errors (parse-opts cmd-args
                                         (concat cli-options
                                                 (cmd-options cmd))
                                         :in-order true))
        opts (merge (:options top-opts) (:options cmd-opts))
        ctx (merge {:weights-res (atom {})}
                   (select-keys opts [:debug :start :namespace :weights]))
        ebnf-grammar (load-grammar (slurp ebnf))

        ;_ (prn :ctx ctx)

        ;; Some additional sanity checks not captured by the CLI parser
        (when (and (= cmd "clj-ns")
                   (not (:namespace ctx)))
          (pr-err "--namespace NAMESPACE required")
          (System/exit 2))

        res (condp = cmd
              "clj-let"
              (println (grammar->generator-let ctx ebnf-grammar))

              "clj-ns"
              (println (grammar->ns ctx ebnf-grammar))

              "gen"
              (doseq [samp (gen/sample (ebnf-gen ctx ebnf-grammar)
                                       (:samples opts))]
                (prn samp))

              "test"
              (let [samples (gen/sample (ebnf-gen ctx ebnf-grammar)
                                        (:iterations opts))]
                (run-tests ctx (:arguments cmd-opts) samples)))]

    (when (:weights-output opts)
      (spit (:weights-output opts)
            (with-out-str (pprint (into (sorted-map) @(:weights-res ctx))))))

    (if (= false res)
      (System/exit 1)
      (System/exit 0))))

