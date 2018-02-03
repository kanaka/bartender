(defproject rend "0.1.0-SNAPSHOT"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :plugins [[lein-localrepo "0.5.3"]]

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)]

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/test.check "0.9.1-SNAPSHOT"]
                 [com.gfredericks/test.chuck "0.2.7"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]

                 [org.clojure/tools.cli "0.3.5"]

                 ;; rend specific
                 [clj-yaml "0.4.0"]
                 [clj-time "0.14.2"]
                 [pandect "0.6.0"]
                 [compojure "1.5.1"]
                 [ring/ring-core "1.5.0"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 ;; Installed with localrepo by running `make deps`
                 [opencv/opencv "2.4.9"]
                 [opencv/opencv-native "2.4.9"]

                 ;; mend specific
                 [hickory "0.7.0"]
                 [instaparse "1.4.5"]
                 [com.rpl/specter "1.0.0"]]

  :profiles {:rend  {:main rend.cli}
             :ebnf  {:main mend.ebnf}
             :css3  {:main mend.css3}
             :html5 {:main mend.html5}}

  :main rend.cli
  )
