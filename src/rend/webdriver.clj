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

(defn init-webdriver [browser]
  (let [url (io/as-url (:url browser))
        capabilities (walk/stringify-keys (or (:capabilities browser) {}))
        caps (DesiredCapabilities. capabilities)
        drv (RemoteWebDriver. url caps)]
    (assoc browser :driver drv)))

(def viewport-margin-script "return [window.outerWidth - document.body.clientWidth, window.outerHeight - document.body.clientHeight];")

(defn set-viewport-size [browser w h]
  (let [drv (:driver browser)
        [mw mh] (.executeScript drv viewport-margin-script
                                (into-array Object []))]
    (-> drv
        (.manage)
        (.window)
        (.setSize (Dimension. (+ w mw) (+ h mh))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(defn addr [cfg]
  (let [host (get cfg :host "localhost")
        port (:port cfg)]
    (str "http://" host ":" port)))

(defn init-session [browser]
  (let [enriched-browser (init-webdriver browser)]
    (set-viewport-size enriched-browser DEFAULT-WIDTH DEFAULT-HEIGHT)
    enriched-browser))

(defn stop-session [browser]
  (.quit (:driver browser)))

(defn load-page [browser url]
  (.get (:driver browser) url))

(defn screenshot-page [browser path]
  (try
    (set-viewport-size browser DEFAULT-WIDTH DEFAULT-HEIGHT)
    (let [sfile (.getScreenshotAs (:driver browser) OutputType/FILE)]
      (io/copy sfile (io/file path))
      (image/imread path))
    (catch Exception e
      (println "Exception:" e)
      (let [eimg (image/error-image
		   DEFAULT-WIDTH DEFAULT-HEIGHT
                   "render failure for" (:id browser))]
	(image/imwrite path eimg)
	eimg))))

