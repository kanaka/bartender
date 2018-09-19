(ns rend.image
  (:import [org.opencv.core Core CvType Mat Scalar Point]
           [org.opencv.highgui Highgui]
           [org.opencv.imgproc Imgproc]))

(def compare-methods {nil             {:alg Imgproc/TM_SQDIFF_NORMED ; default
				       :op <}
		      "SQDIFF"        {:alg Imgproc/TM_SQDIFF
				       :op <}
		      "SQDIFF_NORMED" {:alg Imgproc/TM_SQDIFF_NORMED
				       :op <}
		      "CCORR"         {:alg Imgproc/TM_CCORR
				       :op >}
		      "CCORR_NORMED"  {:alg Imgproc/TM_CCORR_NORMED
				       :op >}
		      "CCOEFF"        {:alg Imgproc/TM_CCOEFF
				       :op >}
		      "CCOEFF_NORMED" {:alg Imgproc/TM_CCOEFF_NORMED
				       :op >}})


(def THUMB-HEIGHT 75)
(def THUMB-WIDTH 100)
;; This needs to match the value (#1289af) in rend.css
(def FILL-COLOR (Scalar. 0xaf 0x89 0x12))

;; Wrap a couple of opencv routines
(defn imread [path]
  (Highgui/imread path))
(defn imwrite [path data]
  (Highgui/imwrite path data))

;; Define some new functions
(defn normalize-images [imgs]
  (let [max-width (apply max (map #(.width %) imgs))
        max-height (apply max (map #(.height %) imgs))]
    (for [img imgs]
      (let [i (Mat/zeros max-height max-width CvType/CV_32FC3)]
        (.convertTo img i CvType/CV_32FC3)
        (Imgproc/copyMakeBorder i i
                                0 (- max-height (.height img))
                                0 (- max-width (.width img))
                                Imgproc/BORDER_CONSTANT
                                FILL-COLOR)
        i))))

;; Based on: http://stackoverflow.com/questions/23342055/how-to-find-mean-averaging-of-pixels-of-15-consecutive-color-images-live-we
;; Images should be normalized by normalize-images first
(defn average [imgs]
  (let [width (.width (first imgs))
        height (.height (first imgs))
        avg (Mat/zeros height width CvType/CV_32FC3)]
    (doseq [img imgs]
      (Core/add avg img avg))
    (Core/divide avg (Scalar/all (count imgs)) avg)
    avg))

(defn thumbnail [img]
  (let [res (Mat/zeros THUMB-HEIGHT THUMB-WIDTH CvType/CV_32FC3)]
    (Imgproc/resize img res (.size res))
    res))

(defn diff [a b alg]
  (let [res (Mat/zeros 0 0 CvType/CV_32FC3)
        d (Imgproc/matchTemplate a b res alg)
        diff (aget (.get res 0 0) 0)]
    diff))

(defn absdiff [a b]
  (let [res (Mat/zeros 0 0 CvType/CV_32FC3)
        d (Core/absdiff a b res)]
    res))

(defn error-image [w h text1 & [text2]]
  (let [res (Mat/zeros h w CvType/CV_32FC3)
        font Core/FONT_HERSHEY_SIMPLEX
        color (Scalar. 0 0 255)]
    (Core/putText res "Error:" (Point. 10 60) font 2 color 2)
    (Core/putText res text1 (Point. 10 150) font 0.7 color 2)
    (when text2
      (Core/putText res text2 (Point. 10 200) font 0.7 color 2))
    res))
