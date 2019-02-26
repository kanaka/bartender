(ns rend.server
  (:require [compojure.core :refer [GET]]
            [compojure.route :as route]
            [org.httpkit.server :refer [run-server with-channel send!
                                        on-close on-receive]]
;            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.util.response :refer [redirect]]))

(def ws-clients
  (atom #{}))

(defn ws-handler [request]
  (with-channel request ch
    (swap! ws-clients conj ch)
    (on-close ch (fn [status]
                   (swap! ws-clients disj ch)
                   (println "channel closed: " status)))
    (on-receive ch (fn [data] ;; echo it back
                     (println "channel" ch "received data:" data)))))

(defn ws-broadcast [data]
  (doseq [ch @ws-clients]
    (send! ch data)))

(defn start-server [cfg default-path]
  (let [port (-> cfg :web :port)
        dir (-> cfg :web :dir)
        routes (compojure.core/routes
                 ;;(GET "/" [] (file-response default-index {}))
                 ;;(GET "/" [] (redirect default-path))
                 (GET "/" [] (str "<html><body>"
                                  "<a href=\"" default-path "\">"
                                  default-path "</a>"
                                  "</body></html>"))
                 (GET "/ws" [] ws-handler)
                 (route/files "/gen" {:root dir})
                 (route/not-found "Not Found"))
        app (-> routes
;                (wrap-defaults site-defaults)
                (wrap-file dir)
                (wrap-content-type)
                (wrap-not-modified))
        server (run-server app {:port port :join? false})]
    (println (str "HTTP kit server started on port: " port
                  ", serving directory: " dir))
    server))
