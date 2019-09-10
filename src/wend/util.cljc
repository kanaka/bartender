(ns wend.util
  (:require [clojure.string :as S]))


(defn strip-wrap-ahem
  [text]
  (S/replace
    text
    #"<span class=\"wrap-ahem\">(.|&#x00c9;)</span>"
    "$1"))
