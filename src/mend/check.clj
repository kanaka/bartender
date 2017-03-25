(ns mend.check
  (:require [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]))


;;(defn qc-try
;;  [seed]
;;  (let [p (prop/for-all [a gen/pos-int] (> (* a a) a))]
;;    (println "start qc")
;;    (tc/quick-check 100 p
;;                    :seed seed
;;                    :max-size 50
;;                    :report-fn (fn [m]
;;                                 (prn :report m)))
;;    (println "finish qc")))

(defn run-check [opts gen-to-check check-fn report-fn]
  (let [{:keys [iterations seed max-size]
	 :or {iterations 10
	      seed 1
	      max-size 50}} opts
	p (prop/for-all* [gen-to-check] check-fn)]
    (tc/quick-check iterations p
		    :seed seed
		    :max-size max-size
		    :reporter-fn report-fn)))


