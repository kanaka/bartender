(defproject rend "0.1.0-SNAPSHOT"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/test.check "0.9.1-SNAPSHOT"]
                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]
                 [clj-yaml "0.4.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [pandect "0.6.0"]
                 [compojure "1.5.1"]
                 [ring/ring-core "1.5.0"]
                 [ring/ring-jetty-adapter "1.5.0"]

                 [net.mikera/imagez "0.11.0"]]

  :source-paths ["src"]

  :main rend.cli
  )
