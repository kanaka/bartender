(ns vend.publish
  (:require [clojure.pprint :refer [pprint]]
	    [cognitect.transit :as transit]
	    [clojure.java.io :as io]

	    [rend.util :refer [merge-test-state]])
  (:import [java.io ByteArrayOutputStream]))


;; Firefox/Chrome
(def ff-chr-res-files
  [
   "40688.edn"
   "80455.edn"
   "59273.edn"
   "82019.edn"
   "48735.edn"
   "59139.edn"
   "31209.edn"
   "67788.edn"
   "61366.edn"
   "66569.edn"
   "80518.edn"
   "28730.edn"
   "23784.edn"
   ])


;; Servo vs Firefox/Chrome
(def servo-res-files
  [
   "61344.edn"
   "58144.edn"
   "55226.edn"
   "44222.edn"
   "90882.edn"
   "53427.edn"
   "10841.edn"
   "991.edn"
   "42525.edn"
   "3999.edn"
   "92163.edn"
   "88278.edn"
   ])

(defn prune-test-state
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

(def WEB-FILES-CFG
  {:servo {:edn-name "servo"
           :suffixes [".html"
                      "_firefox_thumb.png"
                      "_chrome_thumb.png"
                      "_servo_thumb.png"
                      "_diff_firefox_chrome_thumb.png"
                      "_diff_chrome_servo_thumb.png"
                      "_diff_servo_firefox_thumb.png"
                      "_davg_thumb.png"]}
   :ff-chr {:edn-name "firefox-chrome"
            :suffixes [".html"
                       "_firefox_thumb.png"
                       "_chrome_thumb.png"
                       "_diff_firefox_chrome_thumb.png"
                       "_davg_thumb.png"]}})

(def EXTRA-FILES ["dist/app.js"
                  "send.html"
                  "tapv.html"
                  "flat.html"
                  "tabbed.css"
                  "report.css"
                  "modal.css"
                  "tooltip.css"
                  "vend.css"
                  "heydings_controls.ttf"
                  "normalize.css"
                  "rend.css"
                  "Ahem.ttf"])

(defn copy-web-files
  [test-state mode src dst]
  (doseq [f EXTRA-FILES]
    (io/make-parents (str dst "/" f))
    (println "Copying" (str src "/static/" f)
             "to" (str dst "/" f))
    (io/copy (io/file (str src "/static/" f))
             (io/file (str dst "/" f))))
  (let [{:keys [edn-name suffixes]} (get WEB-FILES-CFG mode)
        state (prune-test-state test-state)
        edn-file (str dst "/gen/" edn-name ".edn")
        transit-file (str dst "/gen/" edn-name ".transit")
        out (ByteArrayOutputStream. 33554432)
        writer (transit/writer out :json)
        ;; add violations from smallest iteration in full log
        ;; COMMENTED OUT: not useful for Servo and FF/Chrome
        ;; comparison because always the same.
        #_#_state (loop [state state
                     [[slug run] & log-next] (:log state)]
                (let [slog-file (str src "/gen/" slug "/log.edn")
                      _ (println "Reading" slog-file)
                      slog (read-string (slurp slog-file))
                      iter (:smallest-iter run)
                      violations (get-in slog [:iter-log iter :violations])
                      new-state (assoc-in state
                                          [:log slug :smallest-violations]
                                          violations)]
                  (if log-next
                    (recur new-state log-next)
                    new-state)))]
    (io/make-parents edn-file)
    (spit edn-file state)
    (transit/write writer state)
    (spit transit-file out)
    (doseq [[slug run] (:log state)]
      (let [iter (:smallest-iter run)
            src-prefix (str src "/gen/" slug "/" iter)
            dst-prefix (str dst "/gen/" slug "/" iter)]
        (io/make-parents dst-prefix)
        (doseq [suffix suffixes]
          (println "Copying" (str src-prefix suffix)
                   "to" (str dst-prefix suffix))
          (io/copy (io/file (str src-prefix suffix))
                   (io/file (str dst-prefix suffix))))))))

(comment

(time (def edns (map (comp read-string slurp)
                     (map #(str "gen/" %) ff-chr-res-files))))
(time (def ff-chr-state (apply merge-test-state edns)))

(time (def edns (map (comp read-string slurp)
                     (map #(str "gen/" %) servo-res-files))))
(time (def servo-state (apply merge-test-state edns)))


(time (copy-web-files ff-chr-state :ff-chr "./" "../bartender-gh-pages/"))

(time (copy-web-files servo-state :servo "./" "../bartender-gh-pages/"))

)
