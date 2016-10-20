(ns rend.server
  (:require [cljs.nodejs :as node]))

(def express (node/require "express"))
(def app (express))

(defn start-server [port]
  (doto app
    ;(.use (. express (logger)))
    (.get "/" (fn [req res]
                (.send res "Hello World")))
    (.listen port))
  (println (str "Express server started on port: "
                port
                ;(.port (. app (address)))
                )))

