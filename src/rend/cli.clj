(ns rend.cli
  (:require [mend.check]
            [rend.html5-generators :as html5-gen]
            [rend.server]
            [rend.webdriver :as webdriver]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core :as hiccup]
            [clojure.pprint :refer [pprint]])
  (:import [org.opencv.core Core CvType Mat Scalar]
           [org.opencv.highgui Highgui]
           [org.opencv.imgproc Imgproc]))

(def compare-methods {nil             {:alg Imgproc/TM_SQDIFF_NORMED ; default
                                       :op <}
                      "SQDIFF"        {:alg Imgproc/TM_SQDIFF
                                       :op <}
                      "SQDIFF_NORMED" {:alg Imgproc/TM_SQDIFF_NORMED
                                       :op <}
                      "CCORR"         {:alg Imgproc/TM_CCORR
                                       :op >}
                      "CCORR_NORMED"  {:alg Imgproc/TM_CCORR_NORMED
                                       :op >}
                      "CCOEFF"        {:alg Imgproc/TM_CCOEFF
                                       :op >}
                      "CCOEFF_NORMED" {:alg Imgproc/TM_CCOEFF_NORMED
                                       :op >}})

(def RED "#ff8080")
(def GREEN "#80ff80")

(defn screenshot-page [browser path]
  (let [ss (webdriver/GET browser "screenshot")
        png (base64/decode (.getBytes (:value ss)))
        file (java.io.File. path)]
;    (println "Writing" (count png) "bytes to:" path)
    (clojure.java.io/copy png file)
    png))

(defn normalize-images [imgs]
  (let [max-width (apply max (map #(.width %) imgs))
        max-height (apply max (map #(.height %) imgs))]
    (for [img imgs]
      (let [i (Mat/zeros max-height max-width CvType/CV_32FC3)]
        (.convertTo img i CvType/CV_32FC3)
        (Imgproc/copyMakeBorder i i
                                0 (- max-height (.height img))
                                0 (- max-width (.width img))
                                Imgproc/BORDER_REPLICATE)
        i))))

;; Based on: http://stackoverflow.com/questions/23342055/how-to-find-mean-averaging-of-pixels-of-15-consecutive-color-images-live-we
;; Images should be normalized by normalize-images first
(defn image-average [imgs]
  (let [width (.width (first imgs))
        height (.height (first imgs))
        avg (Mat/zeros height width CvType/CV_32FC3)]
    (doseq [img imgs]
      (Core/add avg img avg))
    (Core/divide avg (Scalar/all (count imgs)) avg)
    avg))

;; Generate an HTML index page for the current test results
(defn index-page [cfg prefix state]
  (let [logs (:log state)
        threshold (-> cfg :compare :threshold)]
    (hiccup/html
      [:html
       [:body
        [:div "Threshold value: " (format "%.6f" threshold)]
        [:br]
        (vec
          (concat
            [:table {:style "border-spacing: 8px 0px"}
             (vec
               (concat
                 [:tr [:th "Test"] [:th "Result"] [:th "Html"]
                  [:th "Average"]]
                 (for [browser (:browsers cfg)]
                   [:th (str (:type browser))])))]
            (for [i (range (count logs))
                  :let [l (nth logs i)
                        idx (+ 1 i)
                        pass (:result l)
                        diffs (:diffs l)
                        violations (:violations l)]]
              (vec
                (concat
                  [:tr
                   [:td idx]
                   [:td
                    {:style (if pass
                              (str "background-color: " GREEN)
                              (str "background-color: " RED))}
                    (if pass "PASS" "FAIL")]
                   [:td [:a {:href (str "/" prefix
                                        "/" idx ".html")
                             :title (str (hiccup/html (:html l)))} "html"]]
                   [:td [:a {:href (str "/" prefix
                                        "/" idx "_avg.png")} "img"]]]
                  (for [browser (:browsers cfg)
                        :let [bdiffs (get diffs browser)]]
                    (vec
                      (concat
                        [:td
                         [:a {:style "padding-left: 2px; padding-right: 2px"
                              :href (str "/" prefix
                                         "/" idx "_" (:type browser) ".png")}
                          "img"]
                         "/ "]
                        (interpose
                          ", "
                          (for [[obrowser odiff] bdiffs]
                            [:span
                             (when (or (get violations browser)
                                     (get violations obrowser))
                               {:style (str "background-color: " RED)})
                             (str (:type obrowser) ": "
                                  (format "%.6f" odiff))]))))))))))]])))


(def check-page-state (atom {}))

(defn check-page* [cfg test-prefix html]
  (let [text (hiccup/html html)
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
      ;; - Get an average result image
      ;; - Calculate a difference value for each image relative to the
      ;;   average.
      ;; - If the difference value is above a threshold, then that is
      ;;   a failed test
      (let [comp-alg (get-in compare-methods [(get-in cfg [:compare :method]) :alg])
            comp-op (get-in compare-methods [(get-in cfg [:compare :method]) :op])
            threshold (get-in cfg [:compare :threshold])
            comp-fn (fn [x] (comp-op threshold x))
            d-path (str test-prefix "_diffs.edn")
            images (into {}
                         (for [browser (:browsers cfg)]
                           (let [ss-path (str test-prefix "_" (:type browser)  ".png")]
                             (screenshot-page browser ss-path)
                             ;; TODO: use PNG return value from
                             ;; screenshot-page
                             [browser (Highgui/imread ss-path)])))
            imgs (apply assoc {}
                        (interleave (keys images)
                                    (normalize-images (vals images))))
            avg (image-average (vals imgs))
            a-path (str test-prefix "_avg.png")
            ; _ (println "Saving average picture to" a-path)
            _ (Highgui/imwrite a-path avg)

            diffs (into
                    {}
                    (for [[browser img] imgs]
                      [browser
                       (into
                         {}
                         (for [[obrowser oimg] (dissoc imgs browser)]
                           (let [res (Mat/zeros 0 0 CvType/CV_32FC3)
                                 d (Imgproc/matchTemplate img oimg res comp-alg)
                                 diff (aget (.get res 0 0) 0)]
                             [obrowser diff])))]))
            ;; at least one difference is greater than threshold
            violation (seq (filter comp-fn (mapcat vals (vals diffs))))
            ;; every difference is greater than threshold
            violations (into {} (filter #(every? comp-fn (vals (val %)))
                                        diffs))]
        ; (println "Saving difference values to" d-path)
        (spit d-path (pr-str diffs))

        ;; Do the actual check
        (println "Threshold violations:" (map (comp :type first) violations))

        [(not violation) diffs violations])
      (catch Throwable t
        (prn :check-page-exception t)
        [false "Exception" nil]))))

;; SIDE-EFFECTS: updates :index in check-page-state atom
(defn new-test-prefix [prefix]
  (let [s (swap! check-page-state update-in [:index] #(+ 1 %))]
    (str prefix "/" (:index s))))

;; SIDE-EFFECTS: updates :log in check-page-state atom
(defn check-page [cfg prefix html]
  (let [test-prefix (new-test-prefix prefix)
        [res diffs violations] (check-page* cfg test-prefix html)
        state (swap! check-page-state update-in [:log]
                     conj {:prefix test-prefix
                           :html html
                           :result res
                           :diffs diffs
                           :violations violations})]
    ;; Generate index page after every check
    (spit (str prefix "/index.html")
          (index-page cfg prefix state))
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
          prefix (str dir "/" id)
          _ (reset! check-page-state {:index 0
                                      :id id
                                      :prefix prefix
                                      :log []})
          qc-res (mend.check/run-check
                   (:quick-check cfg {})
                   html5-gen/gen-html
                   (fn [html] (check-page cfg prefix html))
                   reporter)
          state (swap! check-page-state assoc :final-result qc-res)
          return-code (if (:result qc-res) 0 1)]
      (println "------")
      (println "Quick check results:")
      (pprint qc-res)
      (println "Full results in:" (str prefix "/results.edn"))
      (spit (str prefix "/results.edn") state)
      (System/exit return-code))))

