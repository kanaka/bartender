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

(def check-page-state (atom {}))

(defn check-page* [cfg test-dir test-index html]
  (let [text (hiccup.core/html html)
        test-prefix (str test-dir "/" test-index)
        path (str test-prefix ".html")
        url (str (webdriver/addr (:web cfg)) "/" path)]
    (try
      (println "------")
      ;; (println "Test case:" text)
      ;; (println "Writing to " path)
      (io/make-parents (java.io.File. path))
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

;; SIDE-EFFECTS: updates :index in check-page-state atom
(defn new-test-index []
  (let [s (swap! check-page-state update-in [:index] #(+ 1 %))]
    (:index s)))

;; SIDE-EFFECTS: updates :log in check-page-state atom
(defn check-page [cfg test-dir html]
  (let [test-index (new-test-index)
        [res diffs violations] (check-page* cfg test-dir test-index html)
        state (swap! check-page-state update-in [:log]
                     conj {:prefix (str test-dir "/" test-index)
                           :html html
                           :result res
                           :diffs diffs
                           :violations violations})]
    ;; Generate index page after every check
    (spit (str test-dir "/index.html")
          (rend.html/render-report cfg state))
    res))

;----

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))

(defn pruned-reporter
  "Prune clojure objects from the report data and print it"
  [r]
  (let [r (dissoc r :property)
	r (update-in r [:current-smallest]
		     dissoc :function)]
    (prn :report (dissoc r :property))))


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
        id (rand-int 100000)
        test-dir (str (-> cfg :web :dir) "/" id "-" (-> cfg :quick-check :seed))
        _ (println "Test dir: " test-dir)
        weights (when (:weights cfg)
                  (edn/read-string (slurp (:weights cfg))))
        server (rend.server/start-server cfg)
        browsers (for [browser (:browsers cfg)]
                   (do
                     (println "Initializing browser session for:" (:id browser))
                     (webdriver/init-session browser)))
        ;; Update config with session driver enriched browsers
        cfg (assoc cfg :browsers browsers)]

    (reset! check-page-state {:index 0
                              :id id
                              :test-dir test-dir
                              :log []})
    (let [gen-html (rend.generator/get-html-generator weights)
          start-time (t/now)
          qc-res (instacheck/run-check
                   (:quick-check cfg {})
                   gen-html
                   (fn [html] (check-page cfg test-dir html))
                   pruned-reporter)
          end-time (t/now)
          full-result (swap! check-page-state
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
      (doseq [browser (:browsers cfg)]
        (println "Stopping browser session for:" (:id browser))
        (prn-str (webdriver/stop-session browser)))
      (when (not (:exit-after-run options))
        (println "Continuing to serve on port" (-> cfg :web :port))
        (println "Press <Enter> to exit.")
        (read-line))
      (System/exit return-code))))

