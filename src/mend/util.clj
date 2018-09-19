(ns mend.util)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn inner-text
  "Takes a hickory block and extracts the text content adding
  line-breaks where appropriate (<p> and <br>).
  Returns a sequence of strings
  This is not efficient (some non-TCO), but it works."
  [root]
  (loop [res []
         tree root]
    (cond
      (nil? (seq tree)) res
      (string? tree) [tree]
      (sequential? tree) (recur (concat res (inner-text (first tree)))
                                (next tree))
      (map? tree) (if (get #{:p :br} (:tag tree))
                    (recur (conj res "\n") (:content tree))
                    (recur res (:content tree)))
      :else nil)))

