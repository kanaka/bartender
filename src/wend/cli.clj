(ns wend.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :refer [ends-with?]]
            [clojure.pprint :refer [pprint]]

            [instacheck.core :as instacheck]
            [instacheck.grammar :as igrammar]
            [instacheck.reduce :as ireduce]
            [mend.parse]
            [wend.html-mangle :as wend]))

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
          css-map (wend/extract-css-map text #(slurp (io/file fdir %)))
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

;; The minimal set that can be removed to prevent mutually recursive
;; cycles. Will be added back in at the beginning of the sorted rules.
(def CYCLE-SET #{:stylesheet})

(defn parser-wtrek->ebnf
  [parser wtrek]
  (let [grammar (igrammar/parser->grammar parser)]
    (ireduce/prune-grammar->sorted-ebnf grammar {:wtrek wtrek
                                                 :cycle-set CYCLE-SET})))

;; Default values to use for generator specific weights in order to
;; create a usable generator weight map from a parsed one. Can be
;; either a default value, a weight to lookup, or a function that is
;; called with the wtrek.
(def gen-weight-mapping
  {
   ;; Adjust for different elements in :cat
   [:head-test :cat 3 :opt nil]       #(or (get % [:head :cat 2 :star nil]) 1)
   [:head-test :cat 3 :opt 0]         [:head :cat 2 :star 0]
   [:title-test :cat 1 :star nil]     [:title :cat 1 :star nil]
   [:title-test :cat 1 :star 0]       [:title :cat 1 :star 0]
   [:title-test :cat 3 :star nil]     [:title :cat 3 :star nil]
   [:title-test :cat 3 :star 0]       [:title :cat 3 :star 0]
   [:body-test :cat 1 :star nil]      [:body :cat 1 :star nil]
   [:body-test :cat 1 :star 0]        [:body :cat 1 :star 0]
   [:body-test :cat 4 :star nil]      [:body :cat 3 :star nil]
   [:body-test :cat 4 :star 0]        [:body :cat 3 :star 0]
   [:body-test :cat 4 :star 0 :alt 0] [:body :cat 3 :star 0 :alt 0]
   [:body-test :cat 4 :star 0 :alt 1] [:body :cat 3 :star 0 :alt 1]

   [:css-assignments-test :alt 0]     [:css-assignments :alt 0]
   [:css-assignments-test :alt 1]     [:css-assignments :alt 1]
   [:css-assignments-test :alt 1 :cat 1 :star nil]
   ,,,                                [:css-assignments :alt 1 :cat 2 :star nil]
   [:css-assignments-test :alt 1 :cat 1 :star 0]
   ,,,                                [:css-assignments :alt 1 :cat 2 :star 0]

   ;; No great match, but use the closest elements
   [:char-data-test :alt 0]           [:content :alt 0]
   [:char-data-test :alt 1]           [:content :alt 0]
   [:char-data-test :alt 2]           [:content :alt 1]
   [:char-data-test :alt 3]           [:content :alt 0]

   ;; Just use even weights for url types
   [:url-test :alt 0]                 100
   [:url-test :alt 1]                 100
   [:url-test :alt 2]                 100

   ;; Increase style attr since style and linked stylesheets are not
   ;; used in gen mode. Sum the :element weights to get a decently
   ;; large approx magnitude that is inline with existing weights.
   [:global-attribute :alt 11]
   ,,, (fn [tk] (apply + (vals (filter #(= :element (first (key %))) tk))))
   })

(defn apply-weight-mapping
  [wtrek mapping]
  (reduce
    (fn [tk [p v]]
      (let [w (cond
                (number? v) v
                (fn? v) (v tk)
                :else (get tk v -1))]
        (if (> w 0)
          (assoc tk p w)
          tk)))
    wtrek
    mapping))

;;(defn mangle-wtrek
;;  [orig-wtrek multiplier]
;;  (let [;; Apply the gen-weight-mapping transforms
;;        wtrek1 (apply-weight-mapping orig-wtrek gen-weight-mapping)
;;        ;; Multiply weights by multiplier factor
;;        wtrek2 (into {} (for [[p w] wtrek1]
;;                          [p (* w multiplier)]))
;;        ;; Make sure that global attributes have weight so that style
;;        ;; will appear since style and linked stylesheets are not used
;;        ;; in gen mode.
;;        paths (reduce
;;                (fn [ps [p w]]
;;                  (cond
;;                    (and (ends-with? (first p) "-attribute")
;;                         (not= :global-attribute (first p)))
;;                    (conj ps [(first p) :alt 0])
;;                    :else
;;                    ps))
;;                #{}
;;                wtrek2)
;;        wtrek3 (reduce (fn [tk p]
;;                         (assoc tk p 77 #_(max 100 (or (get tk p) 0))))
;;                       wtrek2
;;                       paths)]
;;    wtrek3))

(defn mangle-wtrek
  [html-grammar orig-wtrek multiplier]
  (let [;; Apply the gen-weight-mapping transforms
        wtrek1 (apply-weight-mapping orig-wtrek gen-weight-mapping)
        ;; Multiply weights by multiplier factor
        wtrek2 (into {} (for [[p w] wtrek1]
                          [p (* w multiplier)]))
        ;; Make sure that global attributes have weight so that style
        ;; will appear since style and linked stylesheets are not used
        ;; in gen mode.
        wtrek3 (reduce
                 (fn [tk [p w]]
                   (if (and (= 3 (count p))
                            (= [:element :alt] (take 2 p)))
                     (let [attr (-> (igrammar/get-in-grammar html-grammar p)
                                    :parsers
                                    (nth 0)
                                    :string
                                    (subs 1)
                                    (str "-attribute")
                                    keyword)]
                       (assoc tk
                              [:element :alt (nth p 2) :cat 1 :star 0] 77
                              [attr :alt 0] 78))
                     tk))
                 wtrek2
                 wtrek2)]
    wtrek3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of wend

(def cli-options
  [[nil "--debug" "Add debug comments to generated code"]
   [nil "--verbose" "Verbose output during execution"]
   [nil "--multiplier MULTIPLIER" "Multiply parsed weights by MULTIPLIER"
    :default 100]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights to WEIGHTS-OUTPUT"]
   [nil "--parse-output PARSE-OUTPUT" "Write resulting parse data to PARSE-OUTPUT"]
   [nil "--html-ebnf-output HTML-EBNF-OUTPUT" "Write pruned HTML EBNF grammar to HTML-EBNF-OUTPUT"]
   [nil "--css-ebnf-output CSS-EBNF-OUTPUT" "Write pruned CSS EBNF grammar to CSS-EBNF-OUTPUT"]])

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
        {:keys [multiplier parse-output weights-output
                html-ebnf-output css-ebnf-output]} (:options opts)
        [& files] (:arguments opts)
        _ (pr-err "Loading HTML parser")
        html-parser (mend.parse/load-parser-from-grammar :html :parse)
        _ (pr-err "Loading CSS parser")
        css-parser (mend.parse/load-parser-from-grammar :css :parse)
        parse-data (parse-files html-parser css-parser files)
        gen-wtrek (mangle-wtrek (:grammar html-parser)
                                (:full-wtrek parse-data)
                                multiplier)]
    (pr-err (str "Combined and filtered weights: "
                 (count gen-wtrek)))
    (when parse-output
      (pr-err (str "Saving parse data to: '" parse-output "'"))
      (spit parse-output parse-data))
    (when html-ebnf-output
      (pr-err (str "Generating pruned HTML EBNF"))
      (let [ebnf (parser-wtrek->ebnf html-parser gen-wtrek)]
        (pr-err (str "Saving pruned HTML EBNF to: '" html-ebnf-output "'"))
        (spit html-ebnf-output ebnf)))
    (when css-ebnf-output
      (pr-err (str "Generating pruned CSS EBNF"))
      (let [ebnf (parser-wtrek->ebnf css-parser gen-wtrek)]
        (pr-err (str "Saving pruned CSS EBNF to: '" css-ebnf-output "'"))
        (spit css-ebnf-output ebnf)))
    (when weights-output
      (pr-err (str "Saving merged weights to: '" weights-output "'"))
      (instacheck/save-weights weights-output gen-wtrek))))
