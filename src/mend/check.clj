(ns mend.check
  (:require [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]))

(defn reporter
  "Prune clojure objects from the report data and print it"
  [r]
  (let [r (dissoc r :property)
        r (update-in r [:current-smallest]
                     dissoc :function)]
    (prn :report (dissoc r :property))))

(defn run-check [opts gen-to-check check-fn report-fn]
  (let [{:keys [iterations seed max-size]
	 :or {iterations 10
	      ;;seed 1
	      max-size 200
              }} opts
	p (prop/for-all* [gen-to-check] check-fn)]
    (tc/quick-check iterations p
		    :seed seed
		    :max-size max-size
		    :reporter-fn report-fn)))


