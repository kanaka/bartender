(ns rend.misc-generators
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hiccup.core :as hiccup]))

(def simple-name
  (gen/fmap #(apply str %1)
            (gen/tuple gen/char-alpha gen/string-alphanumeric)))

(def simple-keyword
  (gen/fmap #(keyword %1) simple-name))

(def percentage
  (gen/fmap #(str % "%")
            (gen/choose 0 100)))

(def hex-color3
  (gen/fmap #(format "#%03x" %)
            (gen/choose 0 0xfff)))

(def hex-color6
  (gen/fmap #(format "#%06x" %)
            (gen/choose 0 0xffffff)))


(def image-path
  (gen/frequency
    [[10 (gen/return "/gen/static/Books-aj.svg_aj_ashton_01g_32x32.png")]
     [5  (gen/return "/gen/static/Books-aj.svg_aj_ashton_01g.png")]
     [10 (gen/return "/gen/static/B_stop.svg")]
     [5  (gen/return "/gen/static/Eilat_-_Dolphin_reef_100x67.jpg")]
     [2  (gen/return "/gen/static/Eilat_-_Dolphin_reef_300x200.jpg")]
     [1  (gen/return "/gen/static/Eilat_-_Dolphin_reef_600x400.jpg")]
     [10 (gen/return "/gen/static/SpaceX_Kestrel_engine2_25x24.gif")]
     [3  (gen/return "/gen/static/SpaceX_Kestrel_engine2.gif")]]))

