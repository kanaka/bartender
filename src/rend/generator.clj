(ns rend.generator
  (:require [rend.css3-generators :as css3-gen]
            [rend.html5-generators :as html5-gen]
            [instacheck.util :as util]
            
            [clojure.test.check.generators :as gen]))

(defn get-html-generator [& [weights]]
  (let [css-gen-map (css3-gen/css3-generators {} weights)
	html-gen-map (html5-gen/html5-generators css-gen-map weights)
	gen-html (gen/fmap util/flatten-text (:html-test html-gen-map))]
    gen-html))


