(ns bartender.cli
  (:require [rend.cli]
            [wend.cli]
            [mend.cli]))

(defn usage []
  (str "Usage: tests [OPTIONS] YAML_CONFIG_FILE
       check-page [OPTIONS] YAML_CONFIG_FILE HTML_FILE
       parse [OPTIONS] HTML_FILE
       mend [OPTIONS]")
  (System/exit 2))

(defn -main [& argv]
  (let [[cmd & args] argv]
    (condp = cmd
      "parse"      (apply wend.cli/-main args)
      "wend"       (apply wend.cli/-main args)
      "tests"      (apply rend.cli/-main (concat args ["--mode" "tests"]))
      "check-page" (apply rend.cli/-main (concat args ["--mode" "check-page"]))
      "mend"       (apply mend.cli/-main args)
      "--help"     (usage)
      (usage))))

