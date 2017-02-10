(ns mend.util
  (:require [clojure.set :refer [union]]
            [alandipert.kahn :as kahn]))


(defn tree-matches
  "Return seq of pred? matches for any node in the tree."
  [pred? tree]
  (let [branch? (some-fn map? sequential?)
        children (fn [n] (if (map? n) (vals n) (seq n)))
        all-nodes (tree-seq branch? children tree)]
    (seq (filter pred? all-nodes))))

(defn tree-deps
  "Takes a structure like {:a tree-a :b tree-b :c tree-c} and returns
  a map like {:a #{:b :c} :b #{:c} :c #{}} which means that :a appears
  in tree-b and tree-c, :b appears in tree-c, but :c does not appear
  in tree-a or tree-b."
  [trees]
  (apply merge-with
         union
         (for [k1 (keys trees)
               [k2 t] trees]
           (if (tree-matches #(= k1 %) t)
             {k2 #{k1}}
             {k2 #{}}))))

;; From: http://stackoverflow.com/a/22800080/471795
(defn topology-sort
  "Takes a dependency map like {:a #{:b :c} ...} where :b and :c are
  the dependencies of :a. Returns a list where the items with no
  dependencies on them appear first in the list and later items can
  only depend on earlier values in the list. Throws an exception if
  the dependency graph if cyclic."
  [m] 
  (let [sorted (kahn/kahn-sort-throws m)]
    (prn :sorted sorted)
    (assert sorted
            (str "Graph is cyclic and cannot be sorted"))
    (reverse sorted)))

(defn select-indexes [coll idxs]
  (keep-indexed #(when ((set idxs) %1) %2) coll))


(comment

(def ttree {:a [1 2 [:b] {:foo [:c :c]}]
            :b {:bar {:baz [:qux :c]}}
            :c {:foo {:bar [:baz :qux []]}}})

(tree-matches #(= :c %) (:a ttree))
;=>(:c :c)

(tree-deps ttree)
;=>{:a #{:b :c} :b #{:c} :c #{}}

(topology-sort (tree-deps ttree))
;=>([:c] [:b] [:a])

)
