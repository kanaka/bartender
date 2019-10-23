(ns send.keeps
  (:require [send.init]
            [vend.init]))

;; This is just a workaround to force compiler optimizations to keep
;; both entry points.
(defn ^:export exports-to-keep [& [mode]]
  (assert (= :never mode) "Do not call exports-to-keep")
  (send.init/send-intro-start)
  (send.init/send-monitor-start)
  (vend.init/tapv-tables-start)
  (vend.init/flat-table-start))
