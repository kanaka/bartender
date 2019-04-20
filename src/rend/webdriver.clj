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
  (prn :url url :capabilities capabilities)
  (let [url (io/as-url url)
        desired-caps (walk/stringify-keys capabilities)
        caps (DesiredCapabilities. desired-caps)
        session (RemoteWebDriver. url caps)]
    session))

;; With an array of two numbers returned, Servo complains about:
;;   {:type org.openqa.selenium.UnsupportedCommandException
;;    :message Unsupported return type}
;;(def viewport-margin-script "return [window.outerWidth - document.body.clientWidth, window.outerHeight - document.body.clientHeight];")

(def viewport-margin-width-script
  "return window.outerWidth - document.body.clientWidth;")
(def viewport-margin-height-script
  "return window.outerHeight - document.body.clientHeight;")

(defn set-viewport-size [session w h]
  (let [mw (.executeScript session viewport-margin-width-script
                           (into-array Object []))
        mh (.executeScript session viewport-margin-height-script
                           (into-array Object []))]
    ;;(prn :mw mw :mh mh)
    (-> session
        (.manage)
        (.window)
        (.setSize (Dimension. (+ w mw) (+ h mh))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-session [url capabilities]
  (let [session (init-webdriver url capabilities)]
    ;;(set-viewport-size session DEFAULT-WIDTH DEFAULT-HEIGHT)
    session))

(defn stop-session [session]
  (.quit session))

(defn load-page [session url]
  (.get session url))

(defn error-image [path msg]
  (let [eimg (image/error-image DEFAULT-WIDTH DEFAULT-HEIGHT msg)]
    (image/imwrite path eimg)
    eimg))

(defn screenshot-page [session path]
  (try
    (set-viewport-size session DEFAULT-WIDTH DEFAULT-HEIGHT)
    (let [sfile (.getScreenshotAs session OutputType/FILE)
          _ (io/copy sfile (io/file path))
          img (image/imread path)]
      (if (= (.width img) 0)
        (do
          (println "Screenshot of" session "returned empty image")
          (error-image path "screenshot error"))
        img))
    (catch Exception e
      (println "Exception:" e)
      (error-image path "render failure"))))

