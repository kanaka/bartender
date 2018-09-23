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

(def check-state (atom {}))

(defn check-page* [cfg test-dir test-index html]
  (let [text (hiccup.core/html html)
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
      (doseq [browser (:browsers cfg)]
        (webdriver/load-page browser url))

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
                         (for [browser (:browsers cfg)]
                           (let [ss-path (str test-prefix "_" (:id browser)  ".png")
                                 img (webdriver/screenshot-page browser ss-path)]
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

;; SIDE-EFFECTS: updates :index in check-state atom
(defn new-test-index []
  (let [s (swap! check-state update-in [:index] #(+ 1 %))]
    (:index s)))

;; SIDE-EFFECTS: updates :log in check-state atom
(defn check-page
  "Render and check a test case page. Then output a report file and
  send an update to any browsers currently viewing the page."
  [cfg test-dir html]
  (let [test-index (new-test-index)
        [res diffs violations] (check-page* cfg test-dir test-index html)
        log-entry {:prefix (str test-dir "/" test-index)
                   :html html
                   :result res
                   :diffs diffs
                   :violations violations}
        state (swap! check-state update-in [:log] conj log-entry)]
    ;; Generate report page after every check and notify browsers
    ;; viewing it of the change.
    (spit (str test-dir "/index.html")
          (rend.html/render-report cfg state))
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
          (rend.html/render-summary state))))
    res))

(defn reporter
  "Prune clojure objects from the report data and print it. If the
  current report type has changed output a report file and send an
  update to any browsers currently viewing the page."
  [cfg test-dir report]
  (let [r (dissoc report :property)
	r (update-in r [:current-smallest]
		     dissoc :function)]
    (when (:verbose cfg)
      (prn :report (merge r {:failing-args :elided :args :elided})))
    ;; Save the latest report
    (swap! check-state
           (fn [s]
             (merge s {:latest-report r}
                    (when (:trial-number r)
                      {:first-fail-number (:trial-number r)
                       :failing-args (:failing-args r)})
                    (when (:current-smallest r)
                      {:smallest (:current-smallest r)})
                    )))
    ))

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
        cfg-file (first arguments)
        cfg (deep-merge (yaml/parse-string (slurp cfg-file))
                        {:verbose (:verbose options)}
                        (if (:seed options)
                          {:quick-check {:seed (:seed options)}}))
        _ (do (println "Configuration:") (pprint cfg))
        weights (when (:weights cfg)
                  (edn/read-string (slurp (:weights cfg))))
        id (rand-int 100000)

        test-dir (str (-> cfg :web :dir) "/" id "-" (-> cfg :quick-check :seed))
        _ (println "Test case/web service dir: " test-dir)
        _ (io/make-parents (java.io.File. (str test-dir "/index.html")))
        ;; Start a web server for the test cases and reporting
        server (rend.server/start-server cfg (str test-dir "/"))
        ;; Set the initial page tracking state
        _ (reset! check-state {:index 0
                               :id id
                               :test-dir test-dir
                               :log []})
        ;; Create the initial empty page
        _ (spit (str test-dir "/index.html")
                (rend.html/render-report cfg @check-state))
        ;; Create webdriver/selenium sessions to the testing browsers
        browsers (for [browser (:browsers cfg)]
                   (do
                     (println "Initializing browser session for:" (:id browser))
                     (webdriver/init-session browser)))
        cleanup-fn (fn [] (doseq [browser browsers]
                            (println "Stopping browser session for:" (:id browser))
                            (prn-str (webdriver/stop-session browser))))
        ;; Update config with session driver enriched browsers
        cfg (assoc cfg :browsers browsers)]

    ;; Run the tests and report
    (let [gen-html (rend.generator/get-html-generator weights)
          start-time (t/now)
          check-fn (partial check-page cfg test-dir)
          reporter-fn (partial reporter cfg test-dir)
          qc-res (instacheck/run-check
                   (:quick-check cfg {})
                   gen-html
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

