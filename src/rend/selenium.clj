(ns rend.selenium
  (:import [org.openqa.selenium
            OutputType Dimension]
           [org.openqa.selenium.remote
            RemoteWebDriver DesiredCapabilities])
  (:require [rend.image :as image]
            [clojure.java.io :as io]))

(def browser-caps
  {:firefox {"moz:firefoxOptions"
             {"args" ["--headless"]}}
   :chrome {"chromeOptions"
            {"args" ["--headless"]}}
   :servo {}})

(defn init-webdriver [browser]
  (let [url (io/as-url (str "http://localhost:" (:port browser)))
        caps (DesiredCapabilities. (get browser-caps (:type browser)))
        drv (RemoteWebDriver. url caps)]
    drv))

(def viewport-margin-script "return [window.outerWidth - document.body.clientWidth, window.outerHeight - document.body.clientHeight];")

(defn set-viewport-size [browser w h]
  (let [drv (:driver browser)
        [mw mh] (.executeScript drv viewport-margin-script 
                                (into-array Object []))]
    (-> drv
        (.manage)
        (.window)
        (.setSize (Dimension. (+ w mw) (+ h mh))))))

(defn addr [& parts])
(def browser-state (atom {}))

(defn screenshot-page [browser path]
  (try
    (let [drv (:driver browser)
          _ (prn :drv drv)
          sfile (.getScreenshotAs drv OutputType/FILE)]
      (prn :sfile sfile)
      (io/copy sfile (io/file path))
      (prn :here3)
      (image/imread path))
    (catch Exception e
      (println "Exception:" e)
      (let [eimg (image/error-image
		   400 300 "render failure for" (:type browser))]
	(image/imwrite path eimg)
	eimg))))

