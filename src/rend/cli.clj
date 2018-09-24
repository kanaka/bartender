(ns rend.cli
  (:require [clojure.tools.cli :refer [parse-opts summarize]]

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

(defn check-page* [cfg state test-index html]
  (let [test-dir (:test-dir cfg)
        sessions (:sessions @state)
        text (hiccup.core/html html)
        test-prefix (str test-dir "/" test-index)
        path (str test-prefix ".html")
        url (str (webdriver/addr (:web cfg)) "/" path)]
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
      (let [comp-alg (get-in image/compare-methods
                             [(get-in cfg [:compare :method]) :alg])
            comp-op (get-in image/compare-methods
                            [(get-in cfg [:compare :method]) :op])
            threshold (get-in cfg [:compare :threshold])
            comp-fn (fn [x] (comp-op threshold x))
            d-path (str test-prefix "_diffs.edn")
            images (into {}
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
        (spit d-path (pr-str diffs))

        ; (println "Saving average picture")
        (let [avg (image/average (vals imgs))
              thumb (image/thumbnail avg)]
          (image/imwrite (str test-prefix "_avg.png") avg)
          (image/imwrite (str test-prefix "_avg_thumb.png") thumb))

        ; (println "Saving difference pictures")
        (doseq [[browser img] imgs]
          (doseq [[obrowser oimg] (dissoc imgs browser)]
            (let [pre (str test-index
                           "_diff_" (:id browser) "_" (:id obrowser))
                  pre2 (str test-index
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
        (println "Threshold violations:" (map (comp :id first) violations))

        [(not violation) diffs violations])
      (catch Throwable t
        (prn :check-page-exception t)
        [false "Exception" nil]))))

;; SIDE-EFFECTS: updates :index in state atom
(defn new-test-index [state]
  (let [s (swap! state update-in [:index] #(+ 1 %))]
    (:index s)))

;; SIDE-EFFECTS: updates :log in state atom, updates the overall
;; report index.html, and broadcasts the updates to any browsers
;; viewing the report document.
(defn check-page
  "Render and check a test case page. Then output a report file and
  send an update to any browsers currently viewing the page."
  [cfg state html]
  (let [test-dir (:test-dir cfg)
        test-index (new-test-index state)
        [res diffs violations] (check-page* cfg state test-index html)
        log-entry {:prefix (str test-dir "/" test-index)
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
        "row:"
        (hiccup.core/html
          (rend.html/render-report-row (:browsers cfg)
                                       log-entry
                                       (- test-index 1)))))
    (rend.server/ws-broadcast
      (str
        "summary:"
        (hiccup.core/html
          (rend.html/render-summary state-val))))
    res))

(defn reporter
  "Prune clojure objects from the report data and print it. If the
  current report type has changed output a report file and send an
  update to any browsers currently viewing the page."
  [cfg state report]
  (let [r (dissoc report :property)
	r (update-in r [:current-smallest]
		     dissoc :function)]
    (when (:verbose cfg)
      (prn :report (merge r {:failing-args :elided :args :elided})))
    ;; Save the latest report
    (swap! state
           (fn [s]
             (merge s {:latest-report r}
                    (when (:trial-number r)
                      {:first-fail-number (:trial-number r)
                       :failing-args (:failing-args r)})
                    (when (:current-smallest r)
                      {:smallest-number (:index s)
                       :smallest (:current-smallest r)}))))))

;----

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))

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
        test-id (rand-int 100000)
        test-dir (str (-> file-cfg :web :dir) "/" test-id "-"
                      (-> file-cfg :quick-check :seed))
        cfg (deep-merge file-cfg
                        {:test-id test-id
                         :test-dir test-dir
                         :verbose (:verbose options)}
                        (if (:seed options)
                          {:quick-check {:seed (:seed options)}}))
        _ (do (println "Configuration:") (pprint cfg))
        weights (when (:weights cfg)
                  (edn/read-string (slurp (:weights cfg))))

        _ (println "Test case/web service dir: " test-dir)
        _ (io/make-parents (java.io.File. (str test-dir "/index.html")))
        ;; Page tracking state
        check-state (atom {:index 0
                           :log []
                           :sessions {}})
        ;; Config and state specific versions of the
        ;; functions/generators used for the actual check process
        check-fn (partial check-page cfg check-state)
        reporter-fn (partial reporter cfg check-state)
        gen-html-fn (rend.generator/get-html-generator weights)
        ;; Cleanup browser sessions on exit
        cleanup-fn (fn []
                     (println "Cleaning up browser sessions")
                     (doseq [[browser session] (:sessions @check-state)]
                       (println "Stopping browser session for:" (:id browser))
                       (try
                         (webdriver/stop-session session)
                         (swap! check-state update-in [:sessions] dissoc browser)
                         (catch Throwable e
                           (println "Failed to stop browser session:" e)))))
        ;; Start a web server for the test cases and reporting
        server (rend.server/start-server cfg (str test-dir "/"))]

    ;; Create the initial empty page
    (spit (str test-dir "/index.html")
          (rend.html/render-report cfg @check-state))

    ;; Create webdriver/selenium sessions to the testing browsers
    (doseq [browser (:browsers cfg)]
      (println "Initializing browser session for:" (:id browser))
      (swap! check-state update-in [:sessions]
             assoc browser (webdriver/init-session browser)))


    ;; Run the tests and report
    (let [start-time (t/now)
          qc-res (instacheck/run-check
                   (:quick-check cfg {})
                   gen-html-fn
                   check-fn
                   reporter-fn)
          end-time (t/now)
          full-result (swap! check-state
                             assoc
                             :start-time (.toDate start-time)
                             :end-time (.toDate end-time)
                             :elapsed-ms (t/in-millis
                                           (t/interval start-time end-time))
                             :final-result qc-res
                             :config cfg)
          return-code (if (:result qc-res) 0 1)]
      (println "------")
      (println (str "Quick check results (also in " test-dir "/results.edn):"))
      (pprint qc-res)
      (println (str "Full results in: " test-dir "/full-results.edn"))
      (spit (str test-dir "/results.edn") (with-out-str (pprint qc-res)))
      (spit (str test-dir "/full-results.edn") full-result)
      (cleanup-fn)
      (when (not (:exit-after-run options))
        (println "Continuing to serve on port" (-> cfg :web :port))
        (println "Press <Enter> to exit.")
        (read-line))
      (System/exit return-code))))

