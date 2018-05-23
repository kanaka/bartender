(ns mend.ebnf
  (:require [clojure.string :as string]
            [clojure.java.io :refer [as-file]]
            [clojure.walk :as walk]
            [clojure.edn :as edn]
            [com.rpl.specter :refer [setval]]

            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]]

            [instaparse.core :as insta]
            [alandipert.kahn :as kahn]

            [mend.util :as util]
            [mend.check]

            ;; Not actually used here, but convenient for testing
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]))

(def RULES-PER-FUNC 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

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
             (for [[idx t] (map-indexed vector (-> tree :parsers))
                   :let [ctx (update-in ctx [:path] conj idx)]]
               (gen-ROUTE ctx t (+ 1 indent))))
           ")"))))


(defn- gen-alt
  "One of the values must occur."
  [{:keys [weights-res weights weights-lookup? path] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE (update-in ctx [:path] conj 0)
                 (-> tree :parsers first) indent)
      (str pre "(gen/frequency [\n"
           (string/join
             "\n"
	     (for [[idx t] (map-indexed vector (-> tree :parsers))
                   :let [path (conj (:path ctx) idx)
                         ctx (assoc ctx :path path)
                         pw (get weights path)
                         weight (if pw pw 100)
                         wcomment (when pw
                                    "    ;; ** adjusted by config ***")]]
               (do
                 (when weights-res
                   (swap! weights-res assoc path weight))
                 (if weights-lookup?
                   (str pre "  [(get weights " path " " weight ")" wcomment "\n"
                        (gen-ROUTE ctx t (+ 2 indent)) "]")
                   (str pre "  [" weight wcomment "\n"
                        (gen-ROUTE ctx t (+ 2 indent)) "]")))))
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
  [{:keys [cur-nt] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))
        kw (:keyword tree)
        kw-ns (namespace kw)
        gen-dict (:gen-dict ctx)]
    (str pre (if (= cur-nt kw)
               "inner"
               (if kw-ns
                 (str kw-ns "/" (name kw))
                 (if gen-dict
                   (str "(:" (name kw) " " gen-dict ")")
                   (str "gen-" (name kw))))))))

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
  [{:keys [debug path] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))
        tag (:tag tree)
        f (get tag-to-gen (:tag tree))
        ;; Update path
        ctx (update-in ctx [:path] conj tag)]
    (assert f (str "No generator found for " tag))
    (str (if debug
           (str pre ";; path: " path "\n")
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

(defn apply-grammar-updates
  "Replace the generators in the grammar as defined by the list of
  {:path PATH :value VALUE} maps in grammar-updates. Find each :path
  in a grammar and replace it with :value (using specter's setval).
  This allows us to replace generators in the grammar with references
  to other generators (such as real generators from a Clojure
  namespace)."
  [grammar grammar-updates]
  (reduce (fn [g {:keys [path value]}]
            (setval path value g))
          grammar grammar-updates))


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

;; Higher level textual generators

(defn grammar->generator-defs-source
  "Takes an grammar (loaded using load-grammar) and returns the text
  of a namespace with top-level defines (defs) for all the rules. If
  the :start is specified in the ctx then this the name of the rule to
  use as the starting rule of the grammar. If :start is not specified
  then the first rule in the grammar file is used as the starting
  rule. Only the start rule flattens the generated values into a final
  string."
  [{:keys [start] :as ctx} grammar]
  (let [grammar (let [gu (:grammar-updates ctx)]
                  (if (fn? gu)
                    (gu ctx grammar)
                    (apply-grammar-updates grammar gu)))
        ordered-rules (check-and-order-rules grammar)
        start (or start (:start (meta grammar)))]
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

(defn grammar->generator-func-source
  "Takes an grammar (loaded using load-grammar) and returns the text
  of a namespace with a single Clojure function. The function takes
  an optional weights map and returns a map of all the rules as
  generators (indexed by rule-name keyword)."
  [{:keys [function] :as ctx} grammar]
  (assert function "No function name specified")
  (let [grammar (let [gu (:grammar-updates ctx)]
                  (if (fn? gu)
                    (gu ctx grammar)
                    (apply-grammar-updates grammar gu)))
        ordered-rules (check-and-order-rules grammar)
        partitioned-rules (map-indexed #(vector %1 %2)
                                       (partition-all RULES-PER-FUNC
                                                      ordered-rules))
        ctx (assoc ctx
                   :weights-lookup? true
                   :gen-dict "gmap")]
    (str
      (string/join
        "\n\n"
        (for [[idx rules] partitioned-rules]
          (str
            "(defn- " function "-part-" idx " [gmap weights]\n"
            "  (let [\n"
            (string/join
              "\n\n"
              (for [k rules
                    :let [v (get grammar k)]]
                (str "        gen-" (name k) "\n"
                     (gen-rule-body ctx k v 4) "\n"
                     "        gmap (assoc gmap " k " gen-" (name k) ")"))) "]\n"
            "    gmap))")))
      (str
        "\n\n"
        "(defn " function " [& [gmap weights]]\n"
        "  (let [gmap (or gmap {})\n"
        (string/join
          "\n"
          (for [[idx _] partitioned-rules]
            (str "        gmap (" function "-part-" idx " gmap weights)"))) "]\n"
        "    gmap))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator object/API

(defn grammar->generator-obj
  [{:keys [start] :as ctx} grammar]
  (let [ctx (assoc ctx :function "ephemeral")
        fn-src (grammar->generator-func-source ctx grammar)
        gen-fn (binding [*ns* (create-ns 'mend.ebnf)]
                 (load-string fn-src))
        start (or start (:start (meta grammar)))
        gens (gen-fn (:weights ctx))
        gen (gen/fmap util/flatten-text (get gens start))]
    (with-meta
      gen
      {:grammar grammar
       :source fn-src
       :function gen-fn
       :context ctx
       :generators gens
       :cur-start start
       :cur-generator gen})))

(defn update-generator-obj
  "Return a new generator object with runtime properties adjusted
  (those that don't require eval of the function source again). Only
  weights and start rule are supported currently."
  [obj & {:keys [weights start]}]
  (let [{:keys [function context cur-start]} (meta obj)
        start (or start cur-start)
        weights (or weights (:weights context))
        gens (function weights)
        gen (gen/fmap util/flatten-text (get gens start))]
    (with-meta
      gen
      (assoc (meta obj)
             :cur-start start
             :cur-generator gen))))

(defn ebnf-gen
  "Takes an path to an EBNF grammar file and return a test.check
  generator. If the start is specified then this the name of the rule
  to use as the starting rule of the grmmar. If start is not specified
  then the first rule in the grammar file is used as the starting
  rule."
  ([ebnf]
   (ebnf-gen {} ebnf))

  ([ctx ebnf]
   (if (map? ebnf)
     (grammar->generator-obj ctx ebnf)
     (grammar->generator-obj ctx (load-grammar ebnf)))))



(comment
  (println (grammar->generator-function-source {} (load-grammar (slurp "test/recur3.ebnf"))))

  (def ebnf-generator (ebnf-gen (slurp "test/recur3.ebnf")))
  (pprint (gen/sample ebnf-generator 10))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage

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
  (str (prefix ctx) (grammar->generator-defs-source ctx grammar)))

(comment
  (def ebnf-grammar (load-grammar (slurp "test/recur1.ebnf")))
  (spit "joel/gen.clj" (grammar->ns {:namespace "joel.gen"}
                                    ebnf-grammar))
)

(defn output-samples
  [ctx raw-path samples]
  (let [subst (re-find #"%" raw-path)
        raw-path (if subst raw-path (str raw-path "%"))]
    (doseq [[idx sample] (map-indexed vector samples)]
      (let [path (string/replace raw-path #"%" (str idx))]
        (println "Generating" path)
        (spit path sample)))))

(defn temp-file [prefix suffix & [dir]]
  (if dir
    (java.io.File/createTempFile prefix suffix (java.io.File. dir))
    (java.io.File/createTempFile prefix suffix)))

(defn run-test
  [ctx raw-cmd sample]
  (let [sample-dir (:sample-dir ctx)
        tmp-file (temp-file "sample" ".data" sample-dir)
        temp-writer (io/writer tmp-file)
        spath (.getCanonicalPath ^java.io.File tmp-file)
        cmd (if (seq (keep #(re-find #"%" %) raw-cmd))
              (map #(string/replace % #"%" spath) raw-cmd)
              (conj raw-cmd spath))
        res (try
              (println "Running:" (string/join " " cmd))
              (.write temp-writer sample)
              (.flush temp-writer)
              (apply sh cmd)
              (finally
                (if (not sample-dir)
                  (.delete tmp-file))))]
    (when (:verbose ctx)
      (println (string/trim-newline
                 (if (= 0 (:exit res))
                   (:out res)
                   (:err res)))))
    (println "Result:"
             (if (= 0 (:exit res))
               "Pass"
               (str "Fail (exit code " (:exit res) ")")))
    (zero? (:exit res))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of ebnf

(def cli-options
  [[nil "--debug" "Add debug comments to generated code"]
   [nil "--verbose" "Verbose output during execution"]
   [nil "--weights WEIGHTS"
    "An EDN data file containing frequency weights (map of ctx path to weight value) to use (default weight is 100)."
    :default nil
    :parse-fn #(edn/read-string (slurp %))]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights out the file."]
   [nil "--start START" "Starting grammar rule"]])

(def cmd-options
  {"clj" [[nil "--namespace NAMESPACE" "Name of namespace to generate"]
          [nil "--function FUNCTION" "Generate a function (that returns a map of generators) rather then top-level generators"]]
   "samples" [[nil "--samples SAMPLES" "Number of samples to generate"
               :default 10]]
   "check" [[nil "--iterations ITERATIONS" "Check/test iterations"
             :default 10]
            [nil "--sample-dir SAMPLE-DIR" "Generate sample files in SAMPLE-DIR and do not delete them on completion"]]})

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(defn usage []
  (pr-err "ebnf [GLOBAL-OPTS] clj     <EBNF-FILE> [CLJ-OPTS]")
  (pr-err "ebnf [GLOBAL-OPTS] samples <EBNF-FILE> [GEN-OPTS] <PATH>")
  (pr-err "ebnf [GLOBAL-OPTS] check   <EBNF-FILE> [CHECK-OPTS] -- <CMD>")
  (System/exit 2))

(defn save-weights [ctx file]
  (when file
    (spit file (with-out-str (pprint (into (sorted-map)
                                           @(:weights-res ctx)))))))

(defn -main
  [& args]
  (let [top-opts (opt-errors (parse-opts args
                                         cli-options :in-order true))
        [cmd ebnf & cmd-args] (:arguments top-opts)
        _ (when (not (and ebnf
                          cmd
                          (#{"clj" "samples" "check"} cmd)))
            (usage))
        cmd-opts (opt-errors (parse-opts cmd-args
                                         (concat cli-options
                                                 (cmd-options cmd))
                                         :in-order true))
        opts (merge (:options top-opts)
                    (into {} (filter (comp not nil? second)
                                     (:options cmd-opts))))
        ctx (merge (select-keys opts [:debug :verbose :start
                                      :namespace :function :weights
                                      :sample-dir :grammar-updates])
                   {:weights-res (atom {})})
        ebnf-grammar (load-grammar (slurp ebnf))

        ;;_ (prn :ctx ctx)

        ;; Some additional sanity checks not captured by the CLI parser
        _ (when (and (= "clj" cmd)
                     (not (:namespace ctx)))
            (pr-err "--namespace NAMESPACE required")
            (System/exit 2))

        _ (when (and (= "samples" cmd)
                     (not (= 1 (count (:arguments cmd-opts)))))
            (usage))

        res (condp = cmd
              "clj"
              (let [gen-src (if (:function opts)
                              grammar->generator-func-source
                              grammar->generator-defs-source)]

                (println
                  (str (prefix ctx)
                       (gen-src ctx ebnf-grammar))))

              "samples"
              (let [samples (gen/sample (ebnf-gen ctx ebnf-grammar)
                                        (Integer. (:samples opts)))]
                (output-samples ctx (first (:arguments cmd-opts)) samples))

              "check"
              (mend.check/run-check
                (select-keys ctx [:iterations])
                (ebnf-gen ctx ebnf-grammar)
                (fn [sample]
                  (run-test ctx
                            (:arguments cmd-opts)
                            sample))
                (fn [r]
                  (when (:verbose ctx)
                    (prn :report (dissoc r :property))))))]

    (when-let [wfile (:weights-output opts)]
      (pr-err "Saving weights to" wfile)
      (save-weights ctx wfile))

    (if (= false res)
      (System/exit 1)
      (System/exit 0))))

