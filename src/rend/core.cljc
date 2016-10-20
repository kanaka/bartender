(ns rend.core
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]))

(defn qc-try
  [seed]
  (let [p (prop/for-all [a gen/pos-int] (> (* a a) a))]
    (println "start qc")
    (tc/quick-check 100 p
                    :seed seed
                    :max-size 50
                    :report-fn (fn [m]
                                 (prn :report m)))
    (print "finish qc")))
