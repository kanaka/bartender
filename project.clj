(defproject rend "0.1.0-SNAPSHOT"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :plugins [[lein-localrepo "0.5.3"]]

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/test.check "0.9.0"]
                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]
                 [clj-yaml "0.4.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [pandect "0.6.0"]
                 [compojure "1.5.1"]
                 [ring/ring-core "1.5.0"]
                 [ring/ring-jetty-adapter "1.5.0"]

                 ;[net.mikera/imagez "0.11.0"]

                 ;; Installed with localrepo by running `make deps`
                 [opencv/opencv "2.4.9"]
                 [opencv/opencv-native "2.4.9"]]

  :source-paths ["src"]

  :main rend.cli
  )
