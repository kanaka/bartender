(ns rend.cli
  (:require [clojure.tools.cli :refer [parse-opts summarize]]

            [instaparse.core :as instaparse]
            [instacheck.core :as instacheck]

            [rend.generator]
            [rend.image :as image]
            [rend.html]
            [rend.server]
            [rend.webdriver :as webdriver]

            [clj-yaml.core :as yaml]
            [clj-time.core :as t]
            [clojure.edn :as edn]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn check-page* [cfg state test-iteration html]
  (let [test-dir (:test-dir cfg)
        sessions (:sessions @state)
        text (hiccup.core/html html)
        test-prefix (str test-dir "/" test-iteration)
        path (str test-prefix ".html")
        host-port (str (get-in cfg [:web :host] "localhost")
                       ":" (get-in cfg [:web :port]))
        url (str "http://" host-port "/" path)
        comp-alg (get-in image/compare-methods
                         [(get-in cfg [:compare :method]) :alg])
        comp-op (get-in image/compare-methods
                        [(get-in cfg [:compare :method]) :op])
        threshold (get-in cfg [:compare :threshold])
        comp-fn (fn [x] (comp-op threshold x))
        d-path (str test-prefix "_diffs.edn")]
    (try
      (println "------")
      ;; (println "Test case:" text)
      ;; (println "Writing to " path)
      (spit path text)
      ;; (println "Writing to " (str path ".txt"))
      ;;(spit (str path ".txt") (rend.html/pprint-html text))
      (spit (str path ".txt") text)

      ;; Load the page in each browser
      (println "Loading" url "in each browser")
      (doseq [[browser session] sessions]
        (webdriver/load-page session url))

      (Thread/sleep 1000)

      ;; - Screenshot each browser
      ;; - Bump up each image size to maximum dimension in each
      ;;   direction (replicating border color)
      ;; - Calculate an average result image and save it to disk
      ;; - Calculate a difference image and summary value for each
      ;;   image relative to the every other image.
      ;; - If any difference value is above a threshold, then that is
      ;;   a failed test
      (let [images (into {}
                         (for [[browser session] sessions]
                           (let [ss-path (str test-prefix "_" (:id browser)  ".png")
                                 img (webdriver/screenshot-page session ss-path)]
                             [browser img])))
            imgs (apply assoc {}
                        (interleave (keys images)
                                    (image/normalize-images (vals images))))

            diffs (into
                    {}
                    (for [[browser img] imgs]
                      [browser
                       (into
                         {}
                         (for [[obrowser oimg] (dissoc imgs browser)]
                           [obrowser (image/diff img oimg comp-alg)]))]))
            ;; at least one difference is greater than threshold
            violation (seq (filter comp-fn (mapcat vals (vals diffs))))
            ;; every difference is greater than threshold
            violations (into {} (filter #(every? comp-fn (vals (val %)))
                                        diffs))]

        ; (println "Saving thumbnail for each screenshot")
        (doseq [[browser img] imgs]
          (let [thumb (image/thumbnail img)]
            (image/imwrite (str test-prefix "_"
                                  (:id browser) "_thumb.png") thumb)))

        ; (println "Saving difference values to" d-path)
        (spit d-path
              (pr-str
                (into {} (for [[b v] diffs]
                           [(:id b) (into {} (for [[bx t] v]
                                               [(:id bx) t]))]))))

        ; (println "Saving average picture")
        (let [avg (image/average (vals imgs))
              thumb (image/thumbnail avg)]
          (image/imwrite (str test-prefix "_avg.png") avg)
          (image/imwrite (str test-prefix "_avg_thumb.png") thumb))

        ; (println "Saving difference pictures")
        (doseq [[browser img] imgs]
          (doseq [[obrowser oimg] (dissoc imgs browser)]
            (let [pre (str test-iteration
                           "_diff_" (:id browser) "_" (:id obrowser))
                  pre2 (str test-iteration
                            "_diff_" (:id obrowser) "_" (:id browser))]
              (if (.exists (io/as-file
                             (str test-dir "/" pre2 ".png")))
                (do
                  (sh "ln" "-sf"
                      (str pre2 ".png")
                      (str test-dir "/" pre ".png"))
                  (sh "ln" "-sf"
                      (str pre2 "_thumb.png")
                      (str test-dir "/" pre "_thumb.png")))
                (let [diff (image/absdiff img oimg)
                      thumb (image/thumbnail diff)]
                  (image/imwrite (str test-dir "/" pre ".png") diff)
                  (image/imwrite (str test-dir "/" pre "_thumb.png") thumb))))))

        ;; Do the actual check
        (when (not (empty? violations))
          (println "Threshold violations:" (map (comp :id first) violations)))

        [(not violation) diffs violations])
      (catch Throwable t
        (prn :check-page-exception t)
        [false "Exception" nil]))))

;; SIDE-EFFECTS: updates :iteration in state atom
(defn new-test-iteration [state]
  (let [s (swap! state update-in [:iteration] #(+ 1 %))]
    (:iteration s)))

;; SIDE-EFFECTS: updates :log in state atom, updates the overall
;; report index.html, and broadcasts the updates to any browsers
;; viewing the report document.
(defn check-page
  "Render and check a test case page. Then output a report file and
  send an update to any browsers currently viewing the page."
  [cfg state html]
  (let [test-id (:test-id cfg)
        test-dir (:test-dir cfg)
        test-iteration (new-test-iteration state)
        [res diffs violations] (check-page* cfg state test-iteration html)
        log-entry {:prefix (str test-dir "/" test-iteration)
                   :html html
                   :result res
                   :diffs diffs
                   :violations violations}
        state-val (swap! state update-in [:log] conj log-entry)]
    ;; Generate report page after every check and notify browsers
    ;; viewing it of the change.
    (spit (str test-dir "/index.html")
          (rend.html/render-report cfg state-val))
    (rend.server/ws-broadcast
      (str
        "row:" test-id ":"
        (hiccup.core/html
          (rend.html/render-report-row (:browsers cfg)
                                       log-entry
                                       (- test-iteration 1)))))
    (rend.server/ws-broadcast
      (str
        "summary:" test-id ":"
        (hiccup.core/html
          (rend.html/render-summary state-val))))
    res))

(defn reporter
  "Prune clojure objects from the report data and print it. If the
  current report type has changed output a report file and send an
  update to any browsers currently viewing the page."
  [cfg state report]
  (let [test-id (:test-id cfg)
        r (dissoc report :property)
	r (update-in r [:current-smallest]
		     dissoc :function)]
    (if (:verbose cfg)
      (prn :report (merge r {:failing-args :elided :args :elided}))
      (println "Report type:" (name (:type r))))
    ;; Save the latest report
    (prn :r r)
    (swap! state
           (fn [s]
             (merge s {:latest-report r}
                    (when (:trial-number r)
                      {:first-fail-number (:trial-number r)
                       :failing-args (:failing-args r)})
                    (when (:current-smallest r)
                      {:smallest-number (:iteration s)
                       :smallest (:current-smallest r)}))))
    ;; Broadcast to listening browsers. We need this here in addition
    ;; to check-page because we may receive multiple reports for the
    ;; same page
    (rend.server/ws-broadcast
      (str
        "summary:" test-id ":"
        (hiccup.core/html
          (rend.html/render-summary @state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: weights

(defn filter-alts
  "Remove paths for weights that are not alternations. Only
  alternations (gen/frequency) are currently affected by the weights
  so remove everything else."
  [weights]
  (into {} (filter #(-> % key reverse second (= :alt)) weights)))

;;(def reducible-regex #"^element$|^[\S-]*-attribute$|^css-assignment$")
(def reducible-regex #"^\[:element :alt [0-9]+\]$|^\[[\S-]*-attribute :alt [0-9]+\]$|^\[css-assignment :alt [0-9]+\]$")
(defn reducer-half [w] (int (/ w 2)))

(defn reduce-weights [weights parser html reducer]
  (let [hparsed (parser html)
        _ (if (instance? instaparse.gll.Failure hparsed)
            (throw (ex-info "Parser failure" {:failure hparsed})))
        wparsed (frequencies (-> hparsed meta :path-log))
        wreduced (for [[p w] (filter-alts wparsed)
                       :when (and (get weights p)
;;                                  (re-seq reducible-regex (name (first p))))]
                                  (re-seq reducible-regex (str p)))]
                   [p (reducer (get weights p))])]
    (into {} wreduced)))

(defn adjust-weights [cfg weights html reducer]
  (let [mode (-> cfg :test :mode)
        weights-full (merge (-> cfg :weights :base) weights)
        parser (:parser cfg)]
    (if (and (= "reduce-weights" mode) html parser)
      (reduce-weights weights-full parser html reducer)
      {})))

(defn save-weights [path weights]
  (spit path (with-out-str (pprint (into (sorted-map) weights)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn run-iterations [cfg test-run-state]
  (let [test-dir (:test-dir cfg)
        weights (:weights @test-run-state)

        ;; Config and state specific versions of the
        ;; functions/generators used for the actual check process
        gen-html-fn (rend.generator/get-html-generator weights)
        check-fn (partial check-page cfg test-run-state)
        reporter-fn (partial reporter cfg test-run-state)]

    ;; Create the initial empty page
    (spit (str test-dir "/index.html")
          (rend.html/render-report cfg @test-run-state))

    (save-weights (str test-dir "/weights-start.edn") weights)

    (let [start-time (t/now)
          qc-res (instacheck/run-check (:quick-check cfg)
                                       gen-html-fn
                                       check-fn
                                       reporter-fn)
          end-time (t/now)

          ;; if requested, adjust weights for next run
          html (-> qc-res :shrunk :smallest first)
          adjusted-weights (adjust-weights cfg weights html reducer-half)
          _ (prn :adjusted-weights adjusted-weights)
          new-weights (merge weights
                             adjusted-weights
                             (-> cfg :weights :fixed))
          _ (prn :new-weights new-weights)

          full-result (swap! test-run-state assoc
                             :start-time (.toDate start-time)
                             :end-time (.toDate end-time)
                             :elapsed-ms (t/in-millis
                                           (t/interval start-time end-time))
                             :final-result qc-res
                             :weights new-weights
                             :config cfg)]
      (println "------")
      (println (str "Quick check results (also in " test-dir "/results.edn):"))
      (pprint qc-res)
      (println (str "Full run results in: " test-dir "/full-results.edn"))
      (spit (str test-dir "/results.edn") (with-out-str (pprint qc-res)))
      (save-weights (str test-dir "/weights-end.edn") new-weights)
      (spit (str test-dir "/full-results.edn") full-result)
      full-result)))


;----

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))

(defn load-parser []
  (let [html5-ebnf (clojure.string/replace
                     (slurp "data/html5.ebnf")
                     #"\n(attr-val-style) = ''\n"
                     "\n$1 = css-assignments ;\n")
        css3-ebnf (slurp "data/css3.ebnf")]
    (instaparse/parser (str html5-ebnf "\n" css3-ebnf))))

(def cli-options
  [["-?" "--help" "Show usage"
    :default false]
   ["-v" "--verbose" "Verbose output"
    :default false]
   ["-s" "--seed SEED" "Test random seed (overrides config file)"
    :parse-fn #(Integer. %)]
   [nil "--exit-after-run" "Exit after a test run rather than pausing for enter."
    :default false]])

(defn usage [data]
  (str "Usage: rend [OPTIONS] YAML_CONFIG_FILE\n"
       (summarize data)))

(defn pr-err [& args] (binding [*out* *err*] (apply println args)))

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  (when (or (-> opts :options :help)
            (-> opts :arguments count (> 1)))
    (println (:summary opts))
    (System/exit 2))
  opts)

(defn -main [& argv]
  (let [{:keys [options arguments]} (opt-errors (parse-opts argv cli-options
                                                            :summary-fn usage))
        cfg-path (first arguments)
        file-cfg (yaml/parse-string (slurp cfg-path))

        ;; Start a web server for the test cases and reporting
        current-dirs-atom (atom [])
        server (rend.server/start-server (-> file-cfg :web :port)
                                         (-> file-cfg :web :dir)
                                         current-dirs-atom)

        ;; If mode is :reduce-weights, then load a parser
        parser (when (= "reduce-weights" (-> file-cfg :test :mode))
                 (println "Loading/creating HTML5 and CSS3 parser")
                 (load-parser))
        weights-base (apply merge
                            (map #(edn/read-string (slurp %))
                                 (-> file-cfg :weights :base)))
        weights-fixed (when (-> file-cfg :weights :fixed)
                        (edn/read-string (slurp (-> file-cfg :weights :fixed))))
        weights-start (when (-> file-cfg :weights :start)
                        (edn/read-string (slurp (-> file-cfg :weights :start))))

        test-id (rand-int 100000)
        base-cfg (merge
                   (deep-merge file-cfg
                               (if (:seed options)
                                 {:quick-check {:seed (:seed options)}}))
                   {:verbose (:verbose options)
                    :parser parser
                    :weights {:base  weights-base
                              :fixed weights-fixed
                              :start weights-start}
                    :test-id test-id})

        ;; Current test run state
        test-run-state (atom {:run 0
                              :iteration 0
                              :log []
                              :sessions {}
                              :weights (merge weights-start weights-fixed)})

        ;; Cleanup browser sessions on exit
        cleanup-fn (fn []
                     (when (not (empty? (:sessions @test-run-state)))
                       (println "Cleaning up browser sessions")
                       (doseq [[browser session] (:sessions @test-run-state)]
                         (println "Stopping browser session for:" (:id browser))
                         (try
                           (webdriver/stop-session session)
                           (swap! test-run-state update-in [:sessions] dissoc browser)
                           (catch Throwable e
                             (println "Failed to stop browser session:" e))))))]

    ;; On Ctrl-C cleanup browser sessions we are about to create
    ;;(.addShutdownHook (Runtime/getRuntime) (Thread. #(do (cleanup-fn)
    ;;                                                     (System/exit 1))))
    (.addShutdownHook (Runtime/getRuntime) (Thread. cleanup-fn))

    ;; Create webdriver/selenium sessions to the testing browsers
    (doseq [browser (:browsers base-cfg)]
      (println "Initializing browser session for:" (:id browser))
      (swap! test-run-state update-in [:sessions] assoc browser
             (webdriver/init-session (:url browser) (or (:capabilities browser) {}))))

    ;; Do the test runs and report
    (loop [run 0]
      (when (< run (-> base-cfg :test :runs))
        (println (str "--- Run " run " -------------------------------------"))
        (swap! test-run-state merge {:run run :iteration 0 :log []})
        (let [seed (if-let [seed (-> base-cfg :quick-check :seed)] (+ seed run))
              test-dir (str (-> base-cfg :web :dir) "/" test-id "-" run "-" seed)
              cfg (deep-merge base-cfg {:test-dir test-dir
                                        :quick-check {:seed seed}})]
          (io/make-parents (java.io.File. (str test-dir "/index.html")))

          ;; Communicate the test directories to the web server
          (swap! current-dirs-atom conj (str test-dir "/"))

          (println "Configuration:")
          (pprint (dissoc (update-in cfg [:weights] dissoc :base) :parser))
          (run-iterations cfg test-run-state)
          (recur (inc run)))))
    (println "\n-----------------------------------------------")
    (cleanup-fn)
    (when (not (:exit-after-run options))
      (println "Continuing to serve on port" (-> base-cfg :web :port))
      (println "Press <Enter> to exit.")
      (read-line))
    (System/exit 0)))

