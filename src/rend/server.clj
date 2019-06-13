(ns rend.server
  (:require [clojure.data.json :as json]
            [compojure.core :refer [GET]]
            [compojure.route :as route]
            [org.httpkit.server :refer [run-server with-channel send!
                                        on-close on-receive]]
;            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.util.response :refer [redirect]]))

;; TODO: make reloadable:
;; https://github.com/ring-clojure/ring/wiki/Reloading

(defonce ws-clients
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
    (send! ch (json/write-str data))))

(defn start-server [port gen-dir root-handler]
  (let [routes (compojure.core/routes
                 ;;(GET "/" [] (file-response default-index {}))
                 ;;(GET "/" [] (redirect default-path))
                 (GET "/" [] root-handler)
                 (GET "/ws" [] ws-handler)
                 (route/files "/gen" {:root gen-dir})
                 (route/files "/static" {:root "static"})
                 (route/not-found "Not Found"))
        app (-> routes
;                (wrap-defaults site-defaults)
                (wrap-file gen-dir)
                (wrap-file "static")
                (wrap-content-type)
                (wrap-not-modified))
        server (run-server app {:port port :join? false})]
    (println (str "HTTP kit server started on port: " port
                  ", serving directory: " gen-dir))
    server))
