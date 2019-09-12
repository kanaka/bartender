(defproject bartender "0.3.0"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :plugins [[lein-localrepo "0.5.3"]
            [lein-cljsbuild "1.1.7"]]

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)]

  :source-paths ["src", "resources"]
  :resource-paths ["resources"]

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.1"]

                 [com.gfredericks/test.chuck "0.2.7"]
                 [com.rpl/specter "1.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/math.combinatorics "0.1.5"]
                 [hickory "0.7.0"]

                 ;; rend specific
                 [clj-yaml "0.4.0"]
                 [clj-time "0.14.2"]
                 [frankiesardo/linked "1.3.0"]
                 [compojure "1.6.1"]
                 [http-kit "2.3.0"]
                 [ring/ring-core "1.7.1"]
                 [ring/ring-devel "1.7.1"] ;; wrap-reload
                 ;; Installed with localrepo by running `make deps`
                 [opencv/opencv "2.4.9"]
                 [opencv/opencv-native "2.4.9"]

                 [org.seleniumhq.selenium/selenium-api "3.141.59"]
                 [org.seleniumhq.selenium/selenium-remote-driver "3.141.59"]
                 [org.seleniumhq.selenium/selenium-server "3.141.59"]

                 [org.clojure/test.check "0.10.0-alpha3"]

                 [kanaka/instacheck "0.8.4"]
                 [kanaka/html5-css3-ebnf "0.6.0"]

                 ;; send/vend
                 [org.clojure/clojurescript "1.10.520"]
                 [org.clojure/core.async "0.4.500"]
                 [com.cemerick/url "0.1.1"]
                 [reagent "0.8.1"]
                 [antizer "0.3.1"]
                 [cljs-http "0.1.46"]
                 [com.cognitect/transit-clj "0.8.313"]
                 [com.cognitect/transit-cljs "0.8.256"]
                 [differ "0.3.2"]]

  :profiles {:rend      {:main rend.cli}
             :mend      {:main mend.cli}
             :wend      {:main wend.cli}
             :rend.core {:main rend.core}
             :send.core {:main send.core}}

  :cljsbuild
  {:builds {:send
            {:source-paths ["src", "resources"]
             :compiler
             {:main          "send.init"
              :asset-path    "/static/build/send/out"
              :output-to     "static/build/send/app.js"
              :output-dir    "static/build/send/out"
              :source-map    true
              :optimizations :none
              :pretty-print  true}}}}

  :main rend.cli)
