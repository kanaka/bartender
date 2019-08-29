(ns rend.misc-generators
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def simple-identifier
  "Generate a simple HTML/CSS identifier (alpha string) that is at
  least one character in length."
  (gen/fmap clojure.string/join
            (gen/let [v (gen/vector gen/char-alpha)
                      itm gen/char-alpha]
              (conj v itm))))

(def floating-point-number
  (gen/double* {:infinite? false :NaN? false}))

;; TODO: undo this when https://github.com/servo/servo/issues/24042
;; is fixed
(def wrap-ahem-char
  (gen/return "<span class=\"wrap-ahem\">X</span>"))
(def wrap-ahem-ref
  (gen/return "<span class=\"wrap-ahem\">&#x00c9;</span>"))

(def image-path
  "Generate a static image path"
  (gen/frequency
    [[10 (gen/return "/static/Books-aj.svg_aj_ashton_01g_32x32.png")]
     ;; Image is darker in Servo (transparency difference?)
     ;;[5  (gen/return "/static/Books-aj.svg_aj_ashton_01g.png")]
     ;; SVG images not support by Servo
     ;;[10 (gen/return "/static/B_stop.svg")]

     [5  (gen/return "/static/Eilat_-_Dolphin_reef_100x67.jpg")]

     ;; Omit large images for now to make wrapping page overflow less
     ;; likely
     ;;[2  (gen/return "/static/Eilat_-_Dolphin_reef_300x200.jpg")]
     ;;[1  (gen/return "/static/Eilat_-_Dolphin_reef_600x400.jpg")]
     [10 (gen/return "/static/SpaceX_Kestrel_engine2_25x24.gif")]
     ;;[3  (gen/return "/static/SpaceX_Kestrel_engine2.gif")]
     ]))
