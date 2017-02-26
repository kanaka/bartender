(defproject mend "0.1.0-SNAPSHOT"
  :description "Generate properties from public data"
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/test.check "0.9.1-SNAPSHOT"]
                 [com.gfredericks/test.chuck "0.2.7"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]

                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]
                 [hickory "0.7.0"]
                 [instaparse "1.4.5"]

                 [org.clojure/tools.cli "0.3.5"]]

  :source-paths ["src"]

  :profiles {:ebnf {:main mend.ebnf}
             :css  {:main mend.css}
             :html {:main mend.html}}

  ;;:main mend.css-download
  ;;:main mend.html-download
  :main mend.ebnf
  )
