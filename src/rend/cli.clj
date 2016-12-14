(ns rend.cli
  (:require [rend.generators]
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
  (let [logs (:log state)]
    (hiccup/html
      [:html
       [:body
        (vec (concat
               [:table {:style "border-spacing: 0px 3px"}
                [:tr [:th "Test"] [:th "Result"] [:th "Html"] [:th "Browsers"] [:th "Average"]]]
               (for [i (range (count logs))
                     :let [l (nth logs i)
                           idx (+ 1 i)
                           pass (:result l)]]
                 [:tr {:style (str "background-color: #" (if pass "88ff88" "ff8888"))}
                  [:td idx]
                  [:td (if pass "PASS" "FAIL")]
                  [:td [:a {:href (str "/" prefix "/" idx ".html")
                            :title (str (hiccup/html (:html l)))} "html"]]
                  (vec (concat
                         [:td]
                         (for [browser (:browsers cfg)]
                           [:a {:style "padding-left: 2px; padding-right: 2px"
                                :href (str "/" prefix "/" idx "_" (:type browser) ".png")} (str (:type browser))])))
                  [:td [:a {:href (str "/" prefix "/" idx "_avg.png")} "avg"]]])))]])))


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

            diffs (into {}
                        (for [[browser img] imgs]
                          (let [res (Mat/zeros 0 0 CvType/CV_32FC3)
                                d (Imgproc/matchTemplate avg img res comp-alg)
                                diff (aget (.get res 0 0) 0)]
                            [browser diff])))
            pixel-cnt (* (.width (first (vals imgs)))
                         (.height (first (vals imgs))))
            violations (filter #(comp-op threshold (val %)) diffs)]
        ; (println "Saving difference values to" d-path)
        (spit d-path (pr-str diffs))

        ;; Do the actual check
        (println "Threshold violations:" (map (comp :type first) violations))

        (if (seq violations)
          false
          true))
      (catch Throwable t
        (prn :check-page-exception t)
        false))))

;; SIDE-EFFECTS: updates :index in check-page-state atom
(defn new-test-prefix [prefix]
  (let [s (swap! check-page-state update-in [:index] #(+ 1 %))]
    (str prefix "/" (:index s))))

;; SIDE-EFFECTS: updates :log in check-page-state atom
(defn check-page [cfg prefix html]
  (let [test-prefix (new-test-prefix prefix)
        res (check-page* cfg test-prefix html)
        state (swap! check-page-state update-in [:log] conj {:html html :result res})]
    ;; Generate index page after every check
    (spit (str prefix "/index.html")
          (index-page cfg prefix state))
    res))

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
;    (rend.generators/qc-try 42)
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
          qc-res (rend.generators/rend-check (:quick-check cfg {})
                                             (fn [html] (check-page cfg prefix html))
                                             (fn [m] (prn :report m)))
          state (swap! check-page-state assoc :final-result qc-res)
          return-code (if (:result qc-res) 0 1)]
      (println "------")
      (println "Quick check results:")
      (pprint qc-res)
      (pprint state)
      ;;(spit (str prefix "/index.html")
      ;;      (index-page cfg prefix state))
      (System/exit return-code))))

