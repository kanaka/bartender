(ns rend.cli
  (:require [rend.generators]
            [rend.server]
            [rend.webdriver :as webdriver]
            [clj-yaml.core :as yaml]
            [clojure.data.codec.base64 :as base64]
            [pandect.algo.sha1 :refer [sha1]]
            [hiccup.core :as hiccup]
            [clojure.pprint :refer [pprint]]
            
            [mikera.image.core :refer [get-pixel]]
            [mikera.image.protocols :as protos]))

(defn screenshot-page [browser path]
  (let [ss (webdriver/GET browser "screenshot")
        png (base64/decode (.getBytes (:value ss)))
        file (java.io.File. path)]
    (println "Writing " (count png) " bytes to: " path)
    (clojure.java.io/copy png file)
    png))


(defn check-page [cfg html]
  (try
    (let [text (hiccup/html html)
          dir (-> cfg :web :dir)
          prefix (str dir "/" (sha1 text))
          path (str prefix ".html")
          url (str (webdriver/addr (:web cfg)) "/" path)]
      (println "Writing to " path ": " text)
      (spit path text)
      ;; Load the page in each browser
      (println "Loading " url " in each browser ")
      (doseq [browser (:browsers cfg)]
        (webdriver/POST browser "url" {"url" url}))
      (Thread/sleep 1000)
      ;; Screenshot each browser
      (let [imgs (for [browser (:browsers cfg)]
                   (let [ss-path (str prefix "_" (:type browser)  ".png")]
                     ;(protos/as-image
                     ;  (screenshot-page browser ss-path))
                     (screenshot-page browser ss-path)
                     (protos/as-image ss-path)
                     ))]
        ;; Compare the images
        (doseq [img imgs]
          (prn :pixel-10-10 (get-pixel img 10 10))
          (prn :pixel-30-30 (get-pixel img 30 30)))
        true))
    (catch Exception e
      (prn :check-page-exception e)
      false)))

(defn -main [& argv]
  (when (= 0 (count argv))
    (println "Usage: rend config.yaml")
    (System/exit 0))
  (let [cfg (yaml/parse-string (slurp (first argv)))]
    (println "Configuration:")
    (pprint cfg)
    (rend.server/start-server cfg)
;    (rend.generators/qc-try 42)
    (doseq [browser (:browsers cfg)]
      (println "Initializing browser session to:" browser)
      (webdriver/init-session browser {}))
    (rend.generators/rend-check (:quick-check cfg {})
                                (fn [html] (check-page cfg html))
                                (fn [m] (prn :report m)))
    (System/exit 0)
    ))

