(ns rend.cli
  (:require [mend.check]
            [rend.html5-generators :as html5-gen]
            [rend.server]
            [rend.webdriver :as webdriver]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core :as hiccup]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :refer [sh]])
  (:import [org.opencv.core Core CvType Mat Scalar]
           [org.opencv.highgui Highgui]
           [org.opencv.imgproc Imgproc]))

;; TODO: move to image lib

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
(def THUMB-HEIGHT 75)
(def THUMB-WIDTH 100)

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

(defn image-thumbnail [img]
  (let [res (Mat/zeros THUMB-HEIGHT THUMB-WIDTH CvType/CV_32FC3)]
    (Imgproc/resize img res (.size res))
    res))


(defn screenshot-page [browser path]
  (let [ss (webdriver/GET browser "screenshot")
        png (base64/decode (.getBytes (:value ss)))
        file (java.io.File. path)]
;    (println "Writing" (count png) "bytes to:" path)
    (clojure.java.io/copy png file)
    ;; TODO: can we convert PNG more directly to image?
    (Highgui/imread path)))

(defn index-page-browser-cell [test-dir idx violations browser browser-diffs]
  [:td
    [:table
     [:tr
      (vec
        (concat
          [:td {:style "vertical-align: top; text-align: center"}
           [:a {:style "padding-left: 2px; padding-right: 2px"
                :href (str "/" test-dir
                           "/" idx "_" (:type browser) ".png")}
            [:img {:src (str "/" test-dir
                             "/" idx "_" (:type browser) "_thumb.png")}]]]
          (for [[obrowser odiff] browser-diffs]
            [:td
             (if (or (get violations browser)
                     (get violations obrowser))
               {:style (str "vertical-align: top; text-align: center; background-color: " RED)}
               {:style (str "vertical-align: top; text-align: center")})
             [:a {:href (str "/" test-dir
                             "/" idx
                             "_diff_" (:type browser)
                             "_" (:type obrowser)
                             ".png")}
              [:img {:src (str "/" test-dir
                               "/" idx
                               "_diff_" (:type browser)
                               "_" (:type obrowser)
                               "_thumb.png")}]]
             [:br]
             (str (:type obrowser) ": "
                  (format "%.6f" odiff))])))]]])

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
         [:td {:style "vertical-align: top"}
          [:a {:href (str "/" test-dir
                          "/" idx "_avg.png")}
           [:img {:src (str "/" test-dir
                            "/" idx "_avg_thumb.png")}]]]
         [:td "&nbsp;"]]
        (for [browser browsers
              :let [bdiffs (get diffs browser)]]
          (index-page-browser-cell test-dir idx
                                   violations
                                   browser bdiffs))))))

;; Generate an HTML index page for the current test results
(defn index-page [cfg test-dir state]
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
                  [:th "Average"] [:th "&nbsp;"]]
                 (for [browser (:browsers cfg)]
                   [:th (str (:type browser))])))]
            (for [i (range (count logs)) ]
              (index-page-test-row test-dir (:browsers cfg) logs i))))]])))


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
      (let [comp-alg (get-in compare-methods [(get-in cfg [:compare :method]) :alg])
            comp-op (get-in compare-methods [(get-in cfg [:compare :method]) :op])
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
                                    (normalize-images (vals images))))

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

        ; (println "Saving thumbnail for each screenshot
        (doseq [[browser img] imgs]
          (let [thumb (image-thumbnail img)]
            (Highgui/imwrite (str test-prefix "_"
                                  (:type browser) "_thumb.png") thumb)))

        ; (println "Saving difference values to" d-path)
        (spit d-path (pr-str diffs))

        ; (println "Saving average picture")
        (let [avg (image-average (vals imgs))
              thumb (image-thumbnail avg)]
          (Highgui/imwrite (str test-prefix "_avg.png") avg)
          (Highgui/imwrite (str test-prefix "_avg_thumb.png") thumb))

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
                (let [diff (Mat/zeros 0 0 CvType/CV_32FC3)
                      _ (Core/absdiff img oimg diff)
                      thumb (image-thumbnail diff)]
                  (Highgui/imwrite (str test-dir "/" pre ".png") diff)
                  (Highgui/imwrite (str test-dir "/" pre "_thumb.png") thumb))))))

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

