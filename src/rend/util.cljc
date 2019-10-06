(ns rend.util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deep-merge [& maps]
  (apply merge-with (fn [x y] (if (map? y) (deep-merge x y) y))
         maps))

(defn merge-test-state
  [cur-state & new-states]
  (loop [cur-state cur-state new-states new-states]
    (let [[new-state & rest-states] new-states
          new-log (merge (:log cur-state)
                         (:log new-state))
          new-slugs (apply conj
                           (:test-slugs new-state)
                           (:test-slugs cur-state))
          cur-state (merge new-state
                           {:log new-log
                            :test-slugs new-slugs})]
      (if (seq rest-states)
        (recur cur-state rest-states)
        cur-state))))
