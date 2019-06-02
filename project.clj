(defproject rend "0.1.0-SNAPSHOT"
  :description "Property based testing of browser rendering engines."
  :url "http://example.com/FIXME"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :plugins [[lein-localrepo "0.5.3"]]

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)]

  :source-paths ["src"]
  :resource-paths ["resources"]

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.1"]

                 [com.gfredericks/test.chuck "0.2.7"]
                 [com.rpl/specter "1.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [hiccup "1.0.5"]
                 [hickory "0.7.0"]

                 ;; rend specific
                 [clj-yaml "0.4.0"]
                 [clj-time "0.14.2"]
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

                 ;; Patched version (retain comments, parsed path log)
                 [kanaka/instaparse "1.4.9.1"]

                 [kanaka/instacheck "0.6.2"]
                 [kanaka/html5-css3-ebnf "0.5.0"]]

  :profiles {:rend      {:main rend.cli}
             :mend      {:main mend.cli}
             :wend      {:main wend.cli}
             :wend.core {:main wend.core}
             :w3c-html5 {:main mend.w3c.html5}
             :w3c-css3  {:main mend.w3c.css3}
             }

  :main rend.core)
