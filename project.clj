(defproject rend "0.1.0-SNAPSHOT"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/test.check "0.9.1-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.1.1"]
            [lein-npm "0.6.2"]]
  
  :npm {:dependencies [[source-map-support "0.4.0"]
                       [request "2.74.0"]
                       [argparse "1.0.7"]
                       [express "4.14.0"]]}

  :source-paths ["src"]

  :cljsbuild {:builds
              [{:source-paths ["src"],
                :compiler {:main rend.cli,
                           :output-to "resources/cli.js",
                           :output-dir "resources/clj/",
                           :target :nodejs,
                           :optimizations :none,
                           :source-map true
                           ;; :foreign-libs [{:file "node_modules/react-bootstrap-table/dist/react-bootstrap-table.js"
                           ;;                 :provides ["react-bootstrap-table"]}]
                           },
                :builds nil}]}

  )
