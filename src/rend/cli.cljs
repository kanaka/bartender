(ns rend.cli
  (:require [cljs.nodejs :as node]
            [rend.core]
            [rend.server]))

(def argparse (node/require "argparse"))

(def ArgumentParser (.-ArgumentParser argparse))

(enable-console-print!)

(defn -main [& args]
  (println "hello world")
  (rend.server/start-server 3000)
  (rend.core/qc-try 42)
  )

(set! *main-cli-fn* -main)
