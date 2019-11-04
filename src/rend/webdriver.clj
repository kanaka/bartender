(ns rend.webdriver
  (:import [org.openqa.selenium
            OutputType Dimension]
           [org.openqa.selenium.remote
            RemoteWebDriver DesiredCapabilities])
  (:require [rend.image :as image]
            [clojure.java.io :as io]
            [clojure.walk :as walk]))

(def DEFAULT-WIDTH 400)
(def DEFAULT-HEIGHT 300)

(defn init-webdriver [url capabilities]
  ;(prn :url url :capabilities capabilities)
  (let [url (io/as-url url)
        desired-caps (walk/stringify-keys capabilities)
        caps (DesiredCapabilities. desired-caps)
        driver (RemoteWebDriver. url caps)]
    driver))

;; With an array of two numbers returned, Servo complains about:
;;   {:type org.openqa.selenium.UnsupportedCommandException
;;    :message Unsupported return type}
;;(def viewport-margin-script "return [window.outerWidth - document.body.clientWidth, window.outerHeight - document.body.clientHeight];")

(def viewport-margin-width-script
  "return window.outerWidth - document.body.clientWidth;")
(def viewport-margin-height-script
  "return window.outerHeight - document.body.clientHeight;")


(defn set-viewport-size [driver w h]
  (-> driver
      (.manage)
      (.window)
      (.setSize (Dimension. w h))))

(defn get-browser-goal-size [driver w h]
  ;; Figure out the window border size
  (set-viewport-size driver w h)
  (let [mw (.executeScript driver viewport-margin-width-script
                           (into-array Object []))
        mh (.executeScript driver viewport-margin-height-script
                           (into-array Object []))]
    {:goal-width  (+ w mw)
     :goal-height (if (< mh 0) h (+ h mh))}))

;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-session [url capabilities]
  (let [driver (init-webdriver url capabilities)
        goal-size (get-browser-goal-size driver DEFAULT-WIDTH DEFAULT-HEIGHT)]
    (merge
      goal-size
      {:driver driver})))

(defn stop-session [{:keys [driver] :as session}]
  (.quit driver))

(defn load-page [{:keys [driver] :as session} url]
  (.get driver url))

(defn error-image [path msg]
  (let [eimg (image/error-image DEFAULT-WIDTH DEFAULT-HEIGHT msg)]
    (image/imwrite path eimg)
    eimg))

(defn screenshot-page [session path]
  (let [{:keys [driver goal-width goal-height]} session]
    (try
      (set-viewport-size driver goal-width goal-height)
      (let [sfile (.getScreenshotAs driver OutputType/FILE)
            _ (io/copy sfile (io/file path))
            img (image/imread path)]
        (if (= (.width img) 0)
          (do
            (println "Screenshot of" driver "returned empty image")
            (error-image path "screenshot error"))
          img))
      (catch Exception e
        (println "Exception:" e)
        (error-image path "render failure")))))

