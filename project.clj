(defproject mend "0.1.0-SNAPSHOT"
  :description "Generate properties from public data"
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 
                 [hickory "0.7.0"]
                 [instaparse "1.4.5"]]

  :source-paths ["src"]

  :main mend.cli
  )
