(ns vend.latex
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as S]

            [rend.util :refer [merge-test-state]]
            [send.util :refer [get-TAP-tree]]))

(defn make-elem-table
  [test-state row-col-slug-tree]
  (let [{:keys [log]} test-state
        rows (sort (set (keys row-col-slug-tree)))
        cols (sort (set (for [r rows
                              c (keys (get row-col-slug-tree r))]
                          c)))
        cell (fn [row col]
               (let [slugs (get-in row-col-slug-tree [row col])]
                 (if (= (count slugs) 0)
                   ""
                   (str (count slugs)))))]
    (str
      "\\begin{tabular}{ |c|" (S/join "|" (repeat (count cols) "c")) "| }\n"
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

(defn table
  [test-state x y]
  (let [cells (get-TAP-tree test-state y x "[None]" "BODY")]
    (make-elem-table test-state cells)))

(defn tables [test-state]
  (let [attrs-tags (get-TAP-tree test-state :attrs :tags "[None]" "BODY")
        props-tags (get-TAP-tree test-state :props :tags "[None]" "BODY")]
    (str
     "Tags & Attributes\n\n"
     (table test-state :tags :attrs)
     "\n\n"
     "Tags & Properties\n\n"
     (table test-state :tags :props))))

(comment

(def res-files
  ["gen/61344.edn"
   "gen/58144.edn"
   "gen/55226.edn"
   "gen/44222.edn"
   "gen/90882.edn"
   "gen/53427.edn"
   ])

(time (def edns (map (comp read-string slurp) res-files)))
(time (def s (apply merge-test-state edns)))

(spit "../paper/paper-defense-servo-tags-attrs.tex" (table s :tags :attrs))
(spit "../paper/paper-defense-servo-tags-props.tex" (table s :tags :props))

)
