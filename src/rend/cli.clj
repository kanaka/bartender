(ns rend.cli
  (:require [clojure.tools.cli :refer [parse-opts summarize]]

            [mend.check]
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

(defn screenshot-page [browser path]
  (try
    (let [ss (webdriver/GET browser "screenshot")
          png (base64/decode (.getBytes (:value ss)))
          ifile (io/file path)]
      ;    (println "Writing" (count png) "bytes to:" path)
      (io/copy png ifile)
      ;; TODO: can we convert PNG more directly to image?
      (image/imread path))
    (catch Exception e
      (let [eimg (image/error-image
                   400 300 "render failure for" (:type browser))]
        (image/imwrite path eimg)
        eimg))))


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
        (webdriver/POST browser "url" {"url" url}))

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
                           (let [ss-path (str test-prefix "_" (:type browser)  ".png")
                                 img (screenshot-page browser ss-path)]
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

        ; (println "Saving thumbnail for each screenshot
        (doseq [[browser img] imgs]
          (let [thumb (image/thumbnail img)]
            (image/imwrite (str test-prefix "_"
                                  (:type browser) "_thumb.png") thumb)))

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
                           "_diff_" (:type browser) "_" (:type obrowser))
                  pre2 (str test-index
                            "_diff_" (:type obrowser) "_" (:type browser))]
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
        (println "Threshold violations:" (map (comp :type first) violations))

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

(defn load-sessions [file]
  (when (and file (.exists (io/as-file file)))
    (println "Sessions keys:")
    (pprint (keys
              (reset! webdriver/browser-state
                      (edn/read-string (slurp file)))))))


;----

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))

(def cli-options
  [["-?" "--help" "Show usage"
    :default false]
   ["-v" "--verbose" "Verbose output"
    :default false]
   ["-S" "--sessions SESSIONS" "Browser sessions file (edn)"
    :default "sessions.edn"]
   ["-s" "--seed SEED" "Test random seed (overrides config file)"
    :parse-fn #(Integer. %)]])

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
        sessions-file (:sessions options)
        cfg (deep-merge (yaml/parse-string (slurp cfg-file))
                        {:verbose (:verbose options)}
                        (if (:seed options)
                          {:quick-check {:seed (:seed options)}}))
        _ (do (println "Configuration:") (pprint cfg))
        id (rand-int 100000)
        test-dir (str (-> cfg :web :dir) "/" id "-" (-> cfg :quick-check :seed))
        _ (println "Test dir: " test-dir)
        weights (when (:weights cfg)
                  (edn/read-string (slurp (:weights cfg))))]
    (when (.exists (io/as-file sessions-file))
      (println "Loading sessions state from" sessions-file)
      (load-sessions sessions-file))
    (rend.server/start-server cfg)
    (doseq [browser (:browsers cfg)]
      (println "Initializing browser session to:" browser)
      (spit sessions-file (prn-str (webdriver/init-session browser {}))))

    (reset! check-page-state {:index 0
                              :id id
                              :test-dir test-dir
                              :log []})
    (let [gen-html (rend.generator/get-html-generator weights)
          start-time (t/now)
          qc-res (mend.check/run-check
                   (:quick-check cfg {})
                   gen-html
                   (fn [html] (check-page cfg test-dir html))
                   mend.check/reporter)
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
      (System/exit return-code))))

