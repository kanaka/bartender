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

(defn summarize-state
  [state]
  (let [log (:log state)
        start-time (:start-time (val (first (sort-by key log))))
        end-time (:end-time (val (last (sort-by key log))))
        elapsed-mses (for [[s run] log] (:elapsed-ms run))
        elapsed-ms (apply + elapsed-mses)
        ;; iter counts per run
        iter-counts (for [[s run] log] (count (:iter-log run)))
        ;; no-failure runs
        successes (filter :result (vals log))
        ;; failure runs
        failures (filter (complement :result) (vals log))
        ;; largest html size for each run
        largest-htmls (for [[s l] log]
                        (apply max (for [[i il] (:iter-log l)]
                                     (count (:html il)))))
        ;; largest html size of each no-failure runs
        largest-success-htmls (for [[s l] log :when (:result l)]
                                (apply max (for [[i il] (:iter-log l)]
                                             (count (:html il)))))]
    {:start-time start-time
     :end-time end-time
     :elapsed-ms elapsed-ms
     :elapsed-hours (float (/ elapsed-ms 3600000))
     :iter-count (apply + iter-counts)
     :successes (count successes)
     :failures (count failures)
     :unique-failures (count (frequencies (map :smallest failures)))
     :largest-html (apply max largest-htmls)
     :iters-per-ms (float (/ (/ elapsed-ms 1000) (apply + iter-counts)))}))
