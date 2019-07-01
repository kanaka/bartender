(ns rend.util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))
