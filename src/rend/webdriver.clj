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

(def viewport-margin-script "return [window.outerWidth - document.body.clientWidth, window.outerHeight - document.body.clientHeight];")

(defn set-viewport-size [session w h]
  (let [[mw mh] (.executeScript session viewport-margin-script
                                (into-array Object []))]
    (-> session
        (.manage)
        (.window)
        (.setSize (Dimension. (+ w mw) (+ h mh))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-session [url capabilities]
  (let [session (init-webdriver url capabilities)]
    (set-viewport-size session DEFAULT-WIDTH DEFAULT-HEIGHT)
    session))

(defn stop-session [session]
  (.quit session))

(defn load-page [session url]
  (.get session url))

(defn screenshot-page [session path]
  (try
    (set-viewport-size session DEFAULT-WIDTH DEFAULT-HEIGHT)
    (let [sfile (.getScreenshotAs session OutputType/FILE)]
      (io/copy sfile (io/file path))
      (image/imread path))
    (catch Exception e
      (println "Exception:" e)
      (let [eimg (image/error-image
		   DEFAULT-WIDTH
                   DEFAULT-HEIGHT
                   "render failure")]
	(image/imwrite path eimg)
	eimg))))

