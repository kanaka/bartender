(ns rend.misc-generators
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hiccup.core :as hiccup]))

(def simple-identifier
  "Generate a simple HTML/CSS identifier (alpha string)"
  (gen/fmap clojure.string/join (gen/vector gen/char-alpha)))

(def image-path
  "Generate a static image path"
  (gen/frequency
    [[10 (gen/return "/static/Books-aj.svg_aj_ashton_01g_32x32.png")]
     [5  (gen/return "/static/Books-aj.svg_aj_ashton_01g.png")]
     [10 (gen/return "/static/B_stop.svg")]
     [5  (gen/return "/static/Eilat_-_Dolphin_reef_100x67.jpg")]
     [2  (gen/return "/static/Eilat_-_Dolphin_reef_300x200.jpg")]
     [1  (gen/return "/static/Eilat_-_Dolphin_reef_600x400.jpg")]
     [10 (gen/return "/static/SpaceX_Kestrel_engine2_25x24.gif")]
     [3  (gen/return "/static/SpaceX_Kestrel_engine2.gif")]]))
