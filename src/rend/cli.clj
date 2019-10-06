(ns rend.cli
  (:require [clojure.tools.cli :refer [parse-opts summarize]]
            [clj-yaml.core :as yaml]
            [clojure.pprint :refer [pprint]]

            [rend.util :as util]
            [rend.core :as core]))

(defn usage [data]
  (str "Usage: rend [OPTIONS] YAML_CONFIG_FILE\n"
       (summarize data)))

(defn pr-err [& args] (binding [*out* *err*] (apply println args)))

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  (when (or (-> opts :options :help)
            (-> opts :arguments count (> 1)))
    (println (:summary opts))
    (System/exit 2))
  opts)

(def cli-options
  [["-?" "--help" "Show usage"
    :default false]
   ["-v" "--verbose" "Verbose output"
    :default false]
   ["-s" "--seed SEED" "Test random seed (overrides config file)"
    :parse-fn #(Integer. %)]
   ["-y" "--no-interactive" "Do not pause for user confirmation before starting tests and before exiting."
    :default false]])

(defn -main [& argv]
  (let [{:keys [options arguments]} (opt-errors (parse-opts argv cli-options
                                                            :summary-fn usage))
        cfg-path (first arguments)
        file-cfg (yaml/parse-string (slurp cfg-path))
        start-seed (or (:seed options) (-> file-cfg :quick-check :start-seed))
        user-cfg (util/deep-merge file-cfg
                                  (when (:verbose options)
                                    {:verbse (:verbose options)})
                                  (when start-seed
                                    {:start-seed start-seed}))
        test-state (core/init-tester user-cfg)]
    (println "Test Configuration:")
    (pprint (:cfg (core/printable-state @test-state)))
    (when (not (:no-interactive options))
      (println "\nPress <Enter> to start tests")
      (read-line))
    (core/run-tests test-state)
    (println "\n-----------------------------------------------")
    (:cleanup-fn test-state)
    (when (not (:no-interactive options))
      (println "\nContinuing to serve on port" (-> user-cfg :web :port))
      (println "Press <Enter> to exit")
      (read-line))
    (System/exit 0)))

