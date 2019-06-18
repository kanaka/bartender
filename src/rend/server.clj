(ns rend.server
  (:require [clojure.data.json :as json]
            [cognitect.transit :as transit]
            [compojure.core :refer [GET]]
            [compojure.route :as route]
            [org.httpkit.server :refer [run-server with-channel send!
                                        on-close on-receive]]
;            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.util.response :refer [redirect]])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

;; TODO: make reloadable:
;; https://github.com/ring-clojure/ring/wiki/Reloading

(defn ws-broadcast [channels data]
  (let [out (ByteArrayOutputStream. 16384)
        writer (transit/writer out :json)
        _ (transit/write writer data)
        string (.toString out)]
    ;;(prn :ws-broadcast :count (count string))
    (doseq [ch channels]
      (send! ch string))))

(defn start-server [port gen-dir open-handler close-handler]
  (let [ws-handler (fn [request]
                      (with-channel request ch
                        (open-handler ch request)
                        (on-close
                          ch (partial close-handler ch))
                        (on-receive
                          ch (fn [data] ;; echo it
                               (println "channel" ch "received data:" data)))))

        routes (compojure.core/routes
                 (GET "/" [] (redirect "/static/index.html"))
                 (GET "/ws" [] ws-handler)
                 (route/files "/gen" {:root gen-dir})
                 (route/files "/static" {:root "static"})
                 (route/not-found "Not Found"))
        app (-> routes
;                (wrap-defaults site-defaults)
                (wrap-file gen-dir)
                ;;(wrap-file "static")
                (wrap-content-type)
                (wrap-not-modified))
        server (run-server app {:port port :join? false})]
    (println (str "HTTP kit server started on port: " port
                  ", serving directory: " gen-dir))
    server))
