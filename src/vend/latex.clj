(ns vend.latex
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]

            [rend.util :refer [merge-test-state]]
            [send.util :refer [get-TAP-tree]]))

(defn make-TAP-matrix-table
  [row-col-slugs-map & [start end]]
  (let [rows (sort (set (keys row-col-slugs-map)))
        rows (if start (drop start rows) rows)
        rows (if (and start end) (take (- end start) rows) rows)
        cols (sort (set (for [r rows
                              c (keys (get row-col-slugs-map r))]
                          c)))
        cell (fn [row col]
               (let [slugs (get-in row-col-slugs-map [row col])]
                 (if (= (count slugs) 0)
                   ""
                   (str (count slugs)))))]
    (println "Row Count:" (count rows))
    (str
      "\\begin{tabular}{ |l|" (S/join "|" (repeat (count cols) "r")) "| }\n"
      "  \\hline\n"
      "  \\rot{ } & " (S/join
			" & "
			(for [col cols]
			 (str "\\rot{" col "}"))) " \\\\\n"
      "  \\hline\n"
      (S/join
        " \\\\\n"
        (for [row rows]
          (str "  " row " & "
               (S/join
                 " & "
                 (for [col cols]
                   (cell row col)))))) " \\\\\n"
      "  \\hline\n"
      "\\end{tabular}\n")))

(defn TAP-matrix-table
  [test-state x y & [start end]]
  (let [cells (get-TAP-tree test-state y x "[None]" "BODY")
        cells (into {} (for [[y v] cells]
                         [y (dissoc v "div" "span")]))]
    (make-TAP-matrix-table cells start end)))

(comment

(def s1 (read-string (slurp "../htmlish/grey-positives-servo10.edn")))

(spit "../paper/paper-defense-servo-tags-attrs.tex"
      (TAP-matrix-table s1 :tags :attrs))
(spit "../paper/paper-defense-servo-tags-props-A.tex"
      (TAP-matrix-table s1 :tags :props 0 46))
(spit "../paper/paper-defense-servo-tags-props-B.tex"
      (TAP-matrix-table s1 :tags :props 46))

)


(defn make-TAP-summary-table
  [col-spec header-names row-data]
  (let [spec (str "|" (S/join "|" col-spec) "|")
        headers (for [h header-names] (str "\\thead{" h "}"))
        rows (for [row row-data] (str "  " (S/join " & " row)))]
    (str
      "\\begin{tabular}{ " spec " }\n"
      "  \\hline\n"
      "  " (S/join " & " headers) " \\\\\n"
      "  \\hline\n"
      (S/join " \\\\\n" rows) " \\\\\n"
      "  \\hline\n"
      "\\end{tabular}\n")))

(defn TAP-summary-table
  [test-state kind kind-name & [start end]]
  (let [failures (filter #(not (:result %))
                         (vals (:log test-state)))
        freqs (frequencies (mapcat #(get-in % [:TAP-summary kind])
                                   failures))
        cells (reverse (sort-by val (dissoc freqs "div" "span")))
        cells (if start (drop start cells) cells)
        cells (if (and start end) (take (- end start) cells) cells)]
    (println "Row Count:" (count cells))
    (make-TAP-summary-table ["l" "r"] [kind-name "Count"] cells)))

(comment

(def s2 (read-string (slurp "../htmlish/grey-positives9.edn")))

(spit "../paper/paper-defense-servo-tags-table.tex"
      (TAP-summary-table s1 :tags "HTML Tag"))
(spit "../paper/paper-defense-servo-attrs-table.tex"
      (TAP-summary-table s1 :attrs "HTML Attribute"))
(spit "../paper/paper-defense-servo-props-table-A.tex"
      (TAP-summary-table s1 :props "CSS Property" 0 46))
(spit "../paper/paper-defense-servo-props-table-B.tex"
      (TAP-summary-table s1 :props "CSS Property" 46))


(spit "../paper/paper-defense-tags-table-A.tex"
      (TAP-summary-table s2 :tags "HTML Tags" 0 53))
(spit "../paper/paper-defense-tags-table-B.tex"
      (TAP-summary-table s2 :tags "HTML Tags" 53))

(spit "../paper/paper-defense-attrs-table.tex"
      (TAP-summary-table s2 :attrs "HTML Attributes"))

(spit "../paper/paper-defense-props-table-A.tex"
      (TAP-summary-table s2 :props "CSS Properties" 0 56))
(spit "../paper/paper-defense-props-table-B.tex"
      (TAP-summary-table s2 :props "CSS Properties" 56 112))
(spit "../paper/paper-defense-props-table-C.tex"
      (TAP-summary-table s2 :props "CSS Properties" 112 168))
(spit "../paper/paper-defense-props-table-D.tex"
      (TAP-summary-table s2 :props "CSS Properties" 168 224))
(spit "../paper/paper-defense-props-table-E.tex"
      (TAP-summary-table s2 :props "CSS Properties" 224 280))
(spit "../paper/paper-defense-props-table-F.tex"
      (TAP-summary-table s2 :props "CSS Properties" 280))

)
