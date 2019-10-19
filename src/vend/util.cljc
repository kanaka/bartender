(ns vend.util)

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

