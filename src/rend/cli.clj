(ns rend.cli
  (:require [mend.check]
            [rend.html5-generators :as html5-gen]
            [rend.image :as image]
            [rend.server]
            [rend.webdriver :as webdriver]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core :as hiccup]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :refer [sh]]
            [clojure.math.combinatorics :refer [combinations]]))

;; TODO: move to image lib

(def RED "#ff8080")
(def GREEN "#80ff80")

(defn screenshot-page [browser path]
  (let [ss (webdriver/GET browser "screenshot")
        png (base64/decode (.getBytes (:value ss)))
        file (java.io.File. path)]
;    (println "Writing" (count png) "bytes to:" path)
    (clojure.java.io/copy png file)
    ;; TODO: can we convert PNG more directly to image?
    (image/imread path)))

(defn index-page-test-row [test-dir browsers logs sth]
  (let [l (nth logs sth)
        idx (+ 1 sth)
        pass (:result l)
        diffs (:diffs l)
        violations (:violations l)
        html (:html l)]
    (vec
      (concat
        [:tr
         [:td idx]
         [:td
          {:style (if pass
                    (str "background-color: " GREEN)
                    (str "background-color: " RED))}
          (if pass "PASS" "FAIL")]
         [:td [:a {:href (str "/" test-dir
                              "/" idx ".html")
                   :title (str (hiccup/html html))}
               "html"]]
         [:td "&nbsp;"]]
        (for [browser browsers]
          [:td {:style "vertical-align: top; text-align: center"}
           [:a {:style "padding-left: 2px; padding-right: 2px"
                :href (str "/" test-dir
                           "/" idx "_" (:type browser) ".png")}
            [:span.tlink "png"]
            [:img.thumb {:style "display: none"
                         :src (str "/" test-dir
                                   "/" idx "_" (:type browser)
                                   "_thumb.png")}]]])
        [[:td "&nbsp;"]
         [:td {:style "vertical-align: top"}
          [:a {:href (str "/" test-dir
                          "/" idx "_avg.png")}
           [:span.tlink "png"]
           [:img.thumb {:style "display: none"
                        :src (str "/" test-dir
                                  "/" idx "_avg_thumb.png")}]]]
         [:td "&nbsp;"]]
        (for [[ba bb] (combinations browsers 2)
              :let [odiff (get-in diffs [ba bb])]]
          [:td
           (if (or (get violations ba)
                   (get violations bb))
             {:style (str "vertical-align: top; text-align: center; background-color: " RED)}
             {:style (str "vertical-align: top; text-align: center")})
           [:a {:href (str "/" test-dir "/" idx
                           "_diff_" (:type ba)
                           "_" (:type bb) ".png")}
            [:img.thumb {:style "display: none"
                         :src (str "/" test-dir "/" idx
                                   "_diff_" (:type ba)
                                   "_" (:type bb) "_thumb.png")}]
            [:br.thumb {:style "display: none"}]
            (format "%.6f" odiff)]])))))

(def toggle-thumbs-js "
  function toggle_thumbs() {
    var toggleb = document.getElementById('toggle');
    var thumb_display = 'none',
        tlink_display = 'none';
    if (toggleb.value === 'Show Thumbnails') {
      toggleb.value = 'Hide Thumbnails'
      thumb_display = 'inline';
    } else {
      toggleb.value = 'Show Thumbnails'
      tlink_display = 'inline';
    }
    for (var x of document.getElementsByClassName('thumb')) {
      x.style.display = thumb_display;
    }
    for (var x of document.getElementsByClassName('tlink')) {
      x.style.display = tlink_display;
    }
  }")

;; Generate an HTML index page for the current test results
(defn index-page [cfg test-dir state]
  (let [logs (:log state)
        threshold (-> cfg :compare :threshold)
        browsers (:browsers cfg)]
    (hiccup/html
      [:html
       [:style "a {text-decoration: none}"]
       [:body
        [:div "Threshold value: " (format "%.6f" threshold)]
        [:br]
        [:input#toggle {:type "button"
                        :value "Show Thumbnails"
                        :onclick "toggle_thumbs()"}]
        [:br][:br]
        (vec
          (concat
            [:table {:style "border-spacing: 4px 0px"}
             (vec
               (concat
                 [:tr [:th "Test"] [:th "Result"] [:th "Html"]
                  [:th "&nbsp;"]]
                 (for [browser browsers]
                   [:th (str (:type browser))])
                 [[:th "&nbsp;"] [:th "Average"] [:th "&nbsp;"]]
                 (for [[ba bb] (combinations browsers 2)]
                   [:th (str (:type ba) "&Delta;" (:type bb))])))]
            (for [i (range (count logs))]
              (index-page-test-row test-dir browsers logs i))))
        [:script toggle-thumbs-js]]])))


(def check-page-state (atom {}))

(defn check-page* [cfg test-dir test-index html]
  (let [text (hiccup/html html)
        test-prefix (str test-dir "/" test-index)
        path (str test-prefix ".html")
        url (str (webdriver/addr (:web cfg)) "/" path)]
    (try
      (println "------")
      (println "Test case:" text)
      ;      (println "Writing to " path)
      (clojure.java.io/make-parents (java.io.File. path))
      (spit path text)

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
              (if (.exists (clojure.java.io/as-file
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
          (index-page cfg test-dir state))
    res))

(defn reporter
  "Prune clojure objects from the report data and print it"
  [r]
  (let [r (dissoc r :property)
        r (update-in r [:current-smallest]
                     dissoc :function)]
    (prn :report (dissoc r :property))))

(defn load-sessions [file]
  (when (and file (.exists (clojure.java.io/as-file file)))
    (println "Sessions keys:")
    (pprint (keys
              (reset! webdriver/browser-state
                      (edn/read-string (slurp file)))))))

(defn -main [& argv]
  (when (= 0 (count argv))
    (println "Usage: rend config.yaml [sessions.edn]")
    (System/exit 0))
  (let [cfg-file (first argv)
        session-file (second argv)
        cfg (yaml/parse-string (slurp cfg-file))]
    (when session-file
      (println "Loading session data")
      (load-sessions session-file))
    (println "Configuration:")
    (pprint cfg)
    (rend.server/start-server cfg)
    (doseq [browser (:browsers cfg)]
      (println "Initializing browser session to:" browser)
      (spit session-file (prn-str (webdriver/init-session browser {}))))
    (let [dir (-> cfg :web :dir)
          id (rand-int 100000)
          test-dir (str dir "/" id)
          _ (reset! check-page-state {:index 0
                                      :id id
                                      :test-dir test-dir
                                      :log []})
          qc-res (mend.check/run-check
                   (:quick-check cfg {})
                   html5-gen/gen-html
                   (fn [html] (check-page cfg test-dir html))
                   reporter)
          state (swap! check-page-state assoc :final-result qc-res)
          return-code (if (:result qc-res) 0 1)]
      (println "------")
      (println "Quick check results:")
      (pprint qc-res)
      (println "Full results in:" (str test-dir "/results.edn"))
      (spit (str test-dir "/results.edn") state)
      (System/exit return-code))))

