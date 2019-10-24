(ns send.util
  (:require [rend.util]
            [clojure.pprint :refer [pprint]]
            #?(:clj [clojure.java.io :as io])))

(defn get-TAP-tree
  [test-state k1 k2 k1-none k2-none]
  (let [{:keys [log test-slugs]} test-state
        TAP-summaries (into {} (for [slug test-slugs]
                                 [slug (get-in log [slug :TAP-summary])]))
        only-k1 (for [[slug tsum] TAP-summaries
                      v1 (get tsum k1)
                      :when (= 0 (count (get tsum k2)))]
                  [v1 k2-none slug])
        only-k2 (for [[slug tsum] TAP-summaries
                      v2 (get tsum k2)
                      :when (= 0 (count (get tsum k1)))]
                  [k1-none v2 slug])
        both (for [[slug tsum] TAP-summaries
                  v1 (get tsum k1)
                  v2 (get tsum k2)]
              [v1 v2 slug])
        tree (reduce
               (fn [t [v1 v2 slug]]
                 (update-in t [v1 v2] (fnil conj []) slug))
               {}
               #_both
               (concat only-k1 only-k2 both))]
    tree))


(defn prune-state
  [test-state]
  (let [log (into {} (for [[slug run] (:log test-state)
                           :when (:smallest-iter run)
                           :let [iter-count (count (:iter-log run))]]
                       [slug (-> run
                                 (dissoc :args :fail :iter-log)
                                 (assoc :iter-count iter-count))]))]
    (-> test-state
        (assoc :log log)
        (dissoc :latest-parse-data :weights))))

(def SUFFIXES ["_firefox_thumb.png"
               "_chrome_thumb.png"
               "_servo_thumb.png"
               "_diff_firefox_chrome_thumb.png"
               "_diff_chrome_servo_thumb.png"
               "_diff_servo_firefox_thumb.png"
               "_davg_thumb.png"])

(def EXTRA-FILES ["dist/app.js"
                  "send.html"
                  "tapv.html"
                  "flat.html"
                  "tabbed.css"
                  "report.css"
                  "vend.css"
                  "heydings_controls.ttf"
                  "normalize.css"
                  "rend.css"
                  "Ahem.ttf"])

#?(:clj

(defn copy-web-files
  [test-state src dst]
  (let [state (prune-state test-state)
        dfile (str dst "/gen/data.edn")]
    (io/make-parents dfile)
    (spit dfile state)
    (doseq [f EXTRA-FILES]
      (io/make-parents (str dst "/" f))
      (io/copy (io/file (str src "/static/" f))
               (io/file (str dst "/" f))))
    (doseq [[slug run] (:log state)]
      (let [iter (:smallest-iter run)
            src-prefix (str src "/gen/" slug "/" iter)
            dst-prefix (str dst "/gen/" slug "/" iter)]
        (io/make-parents dst-prefix)
        (doseq [suffix SUFFIXES]
          (println "Copying" (str src-prefix suffix)
                   "to" (str dst-prefix suffix))
          (io/copy (io/file (str src-prefix suffix))
                   (io/file (str dst-prefix suffix))))))))
)


(comment

(def res-files
  [
   "gen/61344.edn"
   "gen/58144.edn"
   "gen/55226.edn"
   "gen/44222.edn"
   "gen/90882.edn"
   "gen/53427.edn"
   "gen/10841.edn"
   "gen/991.edn"
   "gen/42525.edn"
   "gen/3999.edn"
   ])


(time (def edns (map (comp read-string slurp) res-files)))
(time (def s (apply rend.util/merge-test-state edns)))

(time (copy-web-files s "./" "../bartender-gh-pages/"))

)
