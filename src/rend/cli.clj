(ns rend.cli
  (:require [rend.generators]
            [rend.server]
            [rend.webdriver :as webdriver]
            [clj-yaml.core :as yaml]
            [clojure.data.codec.base64 :as base64]))

(defn -main [& argv]
  (when (= 0 (count argv))
    (println "Usage: rend config.yaml")
    (System/exit 0))
  (let [cfg (yaml/parse-string (slurp (first argv)))]
    (println "main cfg:" cfg)
    (rend.server/start-server cfg)
    (rend.generators/qc-try 42)
    (doseq [browser (:browsers cfg)]
      (println "Contacting: " browser)
;;      (prn :status (webdriver/status browser))
      (prn :post-url (:type browser)
           (webdriver/POST browser "url" {"url" "http://github.com"}))
      (Thread/sleep 1000)
      (prn :title (:type browser)
           (webdriver/GET browser "title"))
      (let [ss (webdriver/GET browser "screenshot")
            png (base64/decode (.getBytes (:value ss)))
            file (java.io.File. (str "screenshot_" (:type browser) ".png"))]
        (prn :screenshot (:type browser) :keys (keys ss) :count (count (:value ss)))
        (clojure.java.io/copy png file)))))

