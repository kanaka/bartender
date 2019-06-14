(ns rend.core
  (:require [clojure.data.codec.base64 :as base64]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.set :as set]

            [flatland.ordered.set :refer [ordered-set]]
            [com.rpl.specter :refer [setval transform MAP-VALS]]
            [hiccup.core]
            [clj-time.core :as ctime]

            [instaparse.core :as instaparse]
            [instacheck.core :as instacheck]

            [mend.util :as util]
            [rend.generator]
            [rend.image :as image]
            [rend.html]
            [rend.server]
            [rend.webdriver :as webdriver]
            [wend.core :as wend]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test state management

(defn new-test-run!
  "Update (mutate) state run number (to add 1) and return it"
  [state]
  (let [s (swap! state update-in [:run] #(+ 1 %))]
    (:run s)))

(defn new-test-iteration!
  "Update (mutate) state iteration number (to add 1) and return it"
  [state]
  (let [s (swap! state update-in [:iteration] #(+ 1 %))]
    (:iteration s)))

(defn new-test-seed!
  "Update (mutate) state seed value (to default if unset, otherwise add 1) and
  return it"
  [state]
  (let [s (swap! state #(let [{:keys [cfg current-seed]} %]
                          (assoc % :current-seed
                                 (if current-seed
                                   (+ 1 current-seed)
                                   (or (:start-seed cfg)
                                       (rand-int 1000000000))))))]
    (:current-seed s)))

(defn add-ws-client!
  [state ch req]
  (swap! state update-in [:ws-clients] conj ch))

(defn remove-ws-client!
  [state ch status]
  (swap! state update-in [:ws-clients] disj ch))


(defn log!
  "Merges (mutates) log entry (map) into the current run and/or iter
  log position (depending on kind).

  The log structure looks like this:
     {:run-log {0 ...
                1 {:run      0
                   ...
                   :iter-log {0 {:iter   0
                                 ...}]}]}}}"
  [state kind entry]
  (swap!
    state
    (fn [{:keys [run iteration] :as data}]
      (condp = kind
        :run (-> data
                 (update-in [:run-log run]
                            merge entry {:run run}))
        :iter (-> data
                  ;; Ensure that parent run-log always has :run key
                  (update-in [:run-log run]
                             merge {:run run})
                  (update-in [:run-log run :iter-log iteration]
                             merge entry {:iter iteration}))))))

(defn printable-state
  "Return a more printable form of the test state the elides: Clojure
  objects/functions, parsers, and long weight lists"
  [state-val]
  (let [cnt-elide (fn [x] (keyword (str "ELIDED-" (count x))))]
    (->> state-val
         (setval    [:sessions MAP-VALS]   :ELIDED)
         (transform [:ws-clients]          cnt-elide)
         (setval    [:server]              :ELIDED)
         (setval    [:cleanup-fn]          :ELIDED)
         (transform [:weights]             cnt-elide)
         (transform [:run-log]             cnt-elide)
         (transform [:cfg :weights :base]  cnt-elide)
         (transform [:cfg :weights :start] cnt-elide)
         (setval    [:html-parser]         :ELIDED)
         (setval    [:css-parser]          :ELIDED))))

(defn update-state!
  "Update the test state by merging (shallow) new-state with the
  following special cases:
    - override :weights with :cfg :weights :fixed weights
    - decrement :current-seed so that it become the next seed"
  [state & [new-state]]
  (printable-state
    (swap! state #(let [wfixed (-> % :cfg :weights :fixed)
                        new-weights (:weights new-state)
                        next-seed (:current-seed new-state)
                        new-sessions (:sessions new-state)]
                    (merge
                      %
                      new-state
                      ;; Fixed weights always override
                      (when new-weights
                        {:weights (merge new-weights wfixed)})
                      ;; If a seed value is specified we
                      ;; decrement so that the next seed is the
                      ;; one specified
                      (when next-seed
                        {:current-seed (dec next-seed)}))))))

(defn reset-state!
  "Reset the test state by setting :run and :iteration to -1, emptying
  :sessions and :run-log, and then call update-state! with the
  new-state"
  [state & [new-state]]
  (update-state! state (merge {:run       -1
                               :iteration -1
                               :sessions  {}
                               :run-log   {}}
                              new-state)))

;; Watch state atom and keep report web page state and file report state
;; in sync.
(defn state-watcher
  "Keep the web page report and file report up-to-date based on the
  changes to the state atom. This includes notifying WebSocket clients
  so that browsers viewing the report page get live updates. There are
  four types of state changes that are handled:
    1. New websocket client connected
    2. New test-slug created (right before quick-check)
    3. New quick-check report received (during quick-check)
    4. A iteration log created (during quick-check, after each check-page)"
  [_key _atom old-state new-state]
  (let [{:keys [cfg ws-clients test-slug run iteration]} new-state
        ws-msg #(rend.server/ws-broadcast ws-clients
                                          {:msgType %1
                                           :data %2
                                           :testSlug test-slug
                                           :iteration iteration})
        changed? #(not= (get-in old-state %) (get-in new-state %))
        ;; a websocket client was added/removed
        new-ws-client (changed? [:ws-clients])
        ;; a new test-slug was just added prior to quick-check
        new-test-slug (changed? [:test-slugs])
        ;; quick-check generated a report during execution
        new-report (changed? [:latest-report])
        ;; check-page finished running a set of quick-check iterations
        new-iter-log (changed? [:run-log run :iter-log])]

    (when (and (or new-ws-client new-test-slug)
               (seq (-> new-state :test-slugs)))
      (ws-msg :newDir (-> new-state :test-slugs)))

    (when (or new-test-slug new-iter-log)
      ;; Generate the full report page when new test dir is created
      ;; (new-test-slug) and after every check-page (new-iter-log).
      (let [test-dir (str (-> cfg :web :dir) "/" test-slug)]
        (spit (str test-dir "/index.html")
              (hiccup.core/html
                (rend.html/render-report new-state)))))

    (when new-iter-log
      ;; We just finished a single check-page call so we know the
      ;; result and can add a new report row.
      (let [log-entry (-> new-state
                          :run-log (get run)
                          :iter-log (get iteration))]
        (ws-msg :row (hiccup.core/html
                       (rend.html/render-report-row (:browsers cfg)
                                                    log-entry
                                                    iteration)))))

    (when (or new-report new-iter-log)
      ;; We need to do this for new reports in addition to each
      ;; iter-log (check-page) because we may receive multiple reports
      ;; for the same page.
      (ws-msg :summary (hiccup.core/html
                         (rend.html/render-summary new-state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lower level testing functions
;; - these may affect file state but not test state atom

(defn qc-report
  "Prune clojure objects from the report data, print it if
  print-full?, and return it."
  [iteration report & [print-full?]]
  (let [r (dissoc report :property)
        r (update-in r [:current-smallest]
                     dissoc :function)
        printed-report (merge r {:failing-args :ELIDED
                                 :args :ELIDED})
        saved-report (merge {:latest-report r}
                            (when (:trial-number r)
                              {:first-fail-number (:trial-number r)
                               :failing-args (:failing-args r)})
                            (when (:current-smallest r)
                              {:smallest-number iteration
                               :smallest (:current-smallest r)}))]
    (if print-full?
      (prn :report printed-report)
      (println "Report type:" (name (:type r))))))

(defn check-page*
  "Utility function for check-page. See check-page."
  [sessions comp-method comp-thresh test-dir test-url iteration html]
  (let [path-prefix (str test-dir "/" iteration)
        url-prefix (str test-url "/" iteration)
        h-path (str path-prefix ".html")
        h-url (str url-prefix ".html")
        comp-alg (get-in image/compare-methods [comp-method :alg])
        comp-op  (get-in image/compare-methods [comp-method :op])
        comp-fn (fn [x] (comp-op comp-thresh x))
        d-path (str path-prefix "_diffs.edn")
        html-text (hiccup.core/html html)]
    (try
      (println "------")
      ;; (println "Test case:" html-text)
      ;; (println "Writing to " path)
      (spit h-path html-text)
      ;; (println "Writing to " (str h-path ".txt"))
      ;;(spit (str h-path ".txt") (rend.html/pprint-html html-text))
      (spit (str h-path ".txt") html-text)

      ;; Load the page in each browser
      (println "Loading" h-url "in each browser")
      (doseq [[browser session] sessions]
        (webdriver/load-page session h-url))

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
                           (let [ss-path (str path-prefix "_" (:id browser)  ".png")
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
            (image/imwrite (str path-prefix "_"
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
          (image/imwrite (str path-prefix "_avg.png") avg)
          (image/imwrite (str path-prefix "_avg_thumb.png") thumb))

        ; (println "Saving difference pictures")
        (doseq [[browser img] imgs]
          (doseq [[obrowser oimg] (dissoc imgs browser)]
            (let [pre1 (str path-prefix
                           "_diff_" (:id browser) "_" (:id obrowser))
                  pre2 (str path-prefix
                            "_diff_" (:id obrowser) "_" (:id browser))]
              (if (and (.exists (io/as-file
                                  (str pre2 ".png")))
                       (not (.exists (io/as-file
                                       (str pre1 ".png")))))
                (do
                  (sh "ln" "-sf"
                      (str (.getName (io/file pre2)) ".png")
                      (str pre1 ".png"))
                  (sh "ln" "-sf"
                      (str (.getName (io/file pre2)) "_thumb.png")
                      (str pre1 "_thumb.png")))
                (let [diff (image/absdiff img oimg)
                      thumb (image/thumbnail diff)]
                  (image/imwrite (str pre1 ".png") diff)
                  (image/imwrite (str pre1 "_thumb.png") thumb))))))

        ;; Do the actual check
        (when (not (empty? violations))
          (println "Threshold violations:" (map (comp :id first) violations)))

        [(not violation) diffs violations])
      (catch Throwable t
        (prn :check-page-exception t)
        [false "Exception" nil]))))

;;(def reducible-regex #"^element$|^[\S-]*-attribute$|^css-declaration$")
(def reducible-regex #"^\[:element :alt [0-9]+\]$|^\[[\S-]*-attribute :alt [0-9]+\]$|^\[:css-known-standard :alt [0-9]+\]$")

(defn reducible-path?
  "Is the path in base-weights and a path that we actually want to
  reduce (based on reducible-regex). Ignores the weight (for now)."
  [base-weights path weight]
  (and (get base-weights path)
       (re-seq reducible-regex (str path))))

(defn reducer-half [w] (int (/ w 2)))

(defn wrap-adjust-weights
  "Call wend/adjust-weights to a get a new set of weights reduced by
  the reducer function."
  [html-parser css-parser weights-full html]
  (let [html-only (wend/extract-html html)
        css-only (wend/extract-inline-css html)
        _ (prn :css-only css-only)
        html-weights (wend/parse-weights html-parser html-only)
        css-weights (wend/parse-weights css-parser css-only)
        adjust-weights (merge-with + html-weights css-weights)]
    (wend/adjust-weights reducible-path? reducer-half
                         weights-full adjust-weights)))

;; TODO: do we really want base weights? It should be implicit in the
;; *_generators.clj code.
(defn load-weights
  "Load the base (multiple), start, and fixed weights paths. Returns
  map containing :base, :start, and :fixed."
  [base-paths fixed-path start-path]
  {:base  (apply merge
                 (map #(edn/read-string (slurp %)) base-paths))
   :start (when start-path (edn/read-string (slurp start-path)))
   :fixed (when fixed-path (edn/read-string (slurp fixed-path)))})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution API

;; SIDE-EFFECTS: updates state atom :test-slugs, :iteration value, and
;; :iter log.
(defn check-page
  "Render and check a test case page. Then output a report file and
  send an update to any browsers currently viewing the page."
  [state extra-cfg test-slug html]
  (let [{:keys [cfg sessions]} @state
        cfg (merge (:cfg @state) extra-cfg)
        test-dir (str (-> cfg :web :dir) "/" test-slug)]

    ;; TODO: combine into a single update
    (update-state! state {:test-slug test-slug})
    ;; Create and communicate the test directories to the web server
    (io/make-parents (str test-dir "/foo"))
    (swap! state update-in [:test-slugs] conj test-slug)

    (let [test-iteration (new-test-iteration! state)
          host-port (str (get-in cfg [:web :host] "localhost")
                         ":" (get-in cfg [:web :port]))
          test-url (str "http://" host-port "/gen/" test-slug)
          comp-method (get-in cfg [:compare :method])
          comp-threshold (get-in cfg [:compare :threshold])
          [res diffs violations] (check-page* sessions
                                              comp-method
                                              comp-threshold
                                              test-dir
                                              test-url
                                              test-iteration
                                              html)
          log-entry {:html html
                     :result res
                     :diffs diffs
                     :violations violations}
          state-val (log! state :iter log-entry)]
      res)))


;; SIDE-EFFECTS: updates state atom run value, seed value, iteration
;; value, run log, and weights
(defn run-iterations
  "Run the quick check process against the test-state and update the
  test-state progressively as the checking happens. If the test mode
  is set to \"reduce-weights\" then the resulting shrunk test case
  will be used to adjust/reduce the weights in the test state for
  subsequent runs."
  [test-state extra-cfg]
  (let [{:keys [cfg test-id weights html-parser css-parser]} @test-state
        cfg (util/deep-merge (-> @test-state :cfg) extra-cfg) ;; add extra-cfg
        run (new-test-run! test-state)
        current-seed (new-test-seed! test-state)
        test-slug (str test-id "-" run "-" current-seed)
        test-dir (str (-> cfg :web :dir) "/" test-slug)]
    (update-state! test-state {:iteration -1})

    (println "Test State:")
    (pprint (printable-state @test-state))

    (wend/save-weights (str test-dir "/weights-start.edn") weights)

    (let [;; Config and state specific versions of the
          ;; functions/generators used for the actual check process
          gen-html-fn (rend.generator/get-html-generator weights)
          check-fn (partial check-page test-state extra-cfg test-slug)
          report-fn (fn [report]
                        (let [{:keys [cfg iteration]} @test-state
                              verbose (:verbose cfg)
                              save (qc-report iteration report verbose)]
                          ;; Save the latest report
                          (update-state! test-state save)))

          start-time (ctime/now)
          ;; ****** QuickCheck ******
          qc-res (instacheck/run-check (merge (:quick-check cfg)
                                              {:seed current-seed})
                                       gen-html-fn
                                       check-fn
                                       report-fn)
          end-time (ctime/now)
          qc-res (merge qc-res
                        {:current-seed current-seed
                         :start-time (.toDate start-time)
                         :end-time (.toDate end-time)
                         :elapsed-ms (ctime/in-millis
                                       (ctime/interval start-time end-time))})
          ;; Merge the quick check results into the run log
          _ (log! test-state :run qc-res)

          ;; if requested, adjust weights for next run
          html (-> qc-res :shrunk :smallest first)
          full-weights (merge (-> cfg :weights :base) weights)
          adjusted-weights (when (and (= "reduce-weights" (-> cfg :test :mode))
                                      html-parser
                                      css-parser
                                      html)
                             (wrap-adjust-weights html-parser
                                                  css-parser
                                                  full-weights
                                                  html))
          new-weights (merge weights adjusted-weights)]
      ;; Update current weights
      (update-state! test-state {:weights new-weights})
      (prn :adjusted-weights adjusted-weights)
      (prn :new-weights new-weights)

      (println "------")
      (println (str "Quick check results (also in " test-dir "/results.edn):"))
      (pprint qc-res)
      (spit (str test-dir "/results.edn") (with-out-str (pprint qc-res)))
      (wend/save-weights (str test-dir "/weights-end.edn") new-weights)
      (println (str "Full run results in: " test-dir "/full-results.edn"))
      (spit (str test-dir "/full-results.edn") @test-state)
      qc-res)))

;; ---

;; SIDE-EFFECTS: updates state atom run value
(defn run-tests
  "Calls run-iterations the number of times specified in the
  configuration (-> cfg :test :runs)."
  [test-state extra-cfg]
  (let [cfg (util/deep-merge (-> @test-state :cfg) extra-cfg)] ;; add extra-cfg
    ;;(update-state! test-state {:run -1})
    ;; Do the test runs and report
    (doseq [run-idx (range (-> cfg :test :runs))]
      (println (str "--- Run index " (inc run-idx) " -----------------"))
      (run-iterations test-state extra-cfg))))

;; ---

(defn init-tester
  "Takes a configuration map (usually loaded from a config.yaml file)
  and returns an initialized the test state atom:
    - Starts a web server for testing and reporting.
    - Loads a html and css parser (if test mode is \"reduce-weights\"
    - Makes a webdriver connection to each browser listed in config.
    - Generates a cleanup functions to tear down browser connections.
  Returns a map containing the config, references the above stateful
  objects, and other settings that are updates as testing progresses."
  [user-cfg & [extra-cfg]]
  (let [user-cfg (util/deep-merge user-cfg extra-cfg) ;; add extra-cfg
        ;; Replace weight paths with loaded weight maps
        weights-map (let [w (-> user-cfg :weights)]
                      (load-weights (:base w) (:fixed w) (:start w)))
        cfg (assoc user-cfg :weights weights-map)
        ;; ws-clients needs to be here early in case client connect
        ;; while parsers are being loaded
        test-state (atom {:ws-clients   #{}})
        ;; Start a web server for the test cases and reporting
        server (do
                 (io/make-parents (str (-> cfg :web :dir) "/foo"))
                 (rend.server/start-server
                   (-> cfg :web :port)
                   (-> cfg :web :dir)
                   (partial #'add-ws-client! test-state)
                   (partial #'remove-ws-client! test-state)))

        ;; If mode is :reduce-weights, then load a parser
        html-parser (when (= "reduce-weights" (-> cfg :test :mode))
                      (println "Loading/creating HTML parser")
                      (wend/load-parser :html-gen-min))
        css-parser (when (= "reduce-weights" (-> cfg :test :mode))
                      (println "Loading/creating CSS parser")
                      (wend/load-parser :css-gen))

        ;; Cleanup browser sessions on exit
        cleanup-fn (fn []
                     (when (not (empty? (:sessions @test-state)))
                       (println "Cleaning up browser sessions")
                       (doseq [[browser session] (:sessions @test-state)]
                         (println "Stopping browser session for:" (:id browser))
                         (try
                           (webdriver/stop-session session)
                           (swap! test-state update-in [:sessions] dissoc browser)
                           (catch Throwable e
                             (println "Failed to stop browser session:" e))))))]

    (reset-state! test-state {:cfg          cfg
                              :test-slugs   (ordered-set)
                              :test-id      (rand-int 100000)
                              :server       server
                              :html-parser  html-parser
                              :css-parser   css-parser
                              :sessions     {}
                              ;; User specified weights (no base)
                              :weights      (:start weights-map)
                              :cleanup-fn   cleanup-fn})

    ;; Attach watcher function to state atom to send updates to any
    ;; listening browser websocket clients
    (add-watch test-state :state-watcher #'state-watcher)

    ;; On Ctrl-C cleanup browser sessions we are about to create
    (.addShutdownHook (Runtime/getRuntime) (Thread. cleanup-fn))

    ;; Create webdriver/selenium sessions to the testing browsers
    (doseq [browser (:browsers cfg)]
      (println "Initializing browser session for:" (:id browser))
      (swap! test-state update-in [:sessions] assoc browser
             (webdriver/init-session (:url browser) (or (:capabilities browser) {}))))

    test-state))

(comment

(require '[clj-yaml.core :as yaml])
(def file-cfg (yaml/parse-string (slurp "config-dev.yaml")))

(time (def test-state (init-tester file-cfg {:verbose true})))

(def res (run-tests test-state {:test {:runs 1} :quick-check {:iterations 3}}))
  ;; OR
(def res (run-iterations test-state {:quick-check {:iterations 3}}))
  ;; OR
(check-page test-state {} "test1" "<html><link rel=\"stylesheet\" href=\"/static/normalize.css\"><link rel=\"stylesheet\" href=\"/static/rend.css\"><body>hello</body></html>")
  ;; OR
(check-page test-state {} "test1" "<html><link rel=\"stylesheet\" href=\"/static/normalize.css\"><link rel=\"stylesheet\" href=\"/static/rend.css\"><body>hello<button></button></body></html>")

;; Do not use :reload-all or it will break ring/rend.server
(require 'wend.core 'rend.core 'rend.html  :reload)

)

(comment

(def adj-test (partial wrap-adjust-weights
                       (-> @test-state :html-parser)
                       (-> @test-state :css-parser)
                       (merge (-> @test-state :cfg :weights :base)
                              (-> @test-state :weights))))

(adj-test "<html><head><link rel=\"stylesheet\" href=\"/static/normalize.css\"><link rel=\"stylesheet\" href=\"/static/rend.css\"><title></title></head><body>x</body></html>")

(adj-test "<html><head><link rel=\"stylesheet\" href=\"/static/normalize.css\"><link rel=\"stylesheet\" href=\"/static/rend.css\"><title></title></head><body>x<div style=\"background-color: red\">X</div></body></html>")

)

