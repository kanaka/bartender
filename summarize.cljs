#!/usr/bin/lumo

(ns rend.summarize
  (:require [cljs.reader :as edn]
            [lumo.io :as io]
            [clojure.string :as S]
            fs
            path
            printf))

(let [top-dir (first *command-line-args*)
      mode (second *command-line-args*)]
  (let [dirs (filter #(re-find #"^[0-9]*-[0-9]*$" %)
                     (fs/readdirSync top-dir))
        res-paths (map #(path/join top-dir % "full-results.edn")
                       dirs)
        res-files (filter fs/existsSync (doall res-paths))
        results (map #(edn/read-string (io/slurp %)) res-files)]
    (doseq [res (sort-by (comp :seed :final-result) results)]
      (let [ms (:elapsed-ms res)
            final (:final-result res)
            shrunk (:shrunk final)
            fail-cnt (count (first (:fail final)))
            shrunk-cnt (count (first (:smallest shrunk)))]
        (println
          (printf "%5d %3d [elapsed: %02d:%02ds (%7d ms), fail steps: %3d, shrink steps: %3d, size: %3d bytes -> %3d bytes (%2.2f%)]"
                  (:id res)
                  (:seed final)
                  (int (/ (/ ms 1000) 60))
                  (int (mod (/ ms 1000) 60))
                  ms
                  (:failing-size final)
                  (:total-nodes-visited shrunk)
                  fail-cnt
                  shrunk-cnt
                  (* 100 (- 1 (/ shrunk-cnt fail-cnt)))))
        (when (= "verbose" mode)
          (println (str
                     "  ... "
                     (S/replace (first (:smallest shrunk)) #"^<html>.*?(<body.*)</html>$" "$1")
                     " ...")))))))
