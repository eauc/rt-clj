; # Canvas

(ns rt-clj.canvas
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [clojure.string :as st]
            [rt-clj.colors :as c]
            [uncomplicate.neanderthal.core :as nc]))

; ## Creation

; A canvas is just a rectangular grid of pixels.
; - Every pixel in the canvas should be initialized to black by default.
; - The canvas constructor also accept a custom initialization color.

; We choose to use simple vectors of rows to store the canvas.

(defn canvas
  ([w h col]
   (let [row (vec (repeat w col))]
     (vec (repeat h row))))
  ([w h]
   (canvas w h (c/color 0. 0. 0.))))

(defn width [c]
  (count (first c)))

(defn height [c]
  (count c))

(defn pixels [c]
  (reduce concat c))

; We can write & read pixels at specific positions in canvas.

(defn assoc-at [c x y p]
  (assoc-in c [y x] p))

(defn get-at [c x y]
  (get-in c [y x]))

; ## PPM image format

; We can transform the canvas to a PPM image file format.

; No line in a PPM file should be more than 70 characters long.

(def ppm-max-line-length 70)

; The first 3 rows of the file are the header:
; - the `"P3"` magic number.
; - the `width` and `height` separated by a space.
; - the maximum color value.

(defn ppm-header [cv]
  ["P3"
   (str (width cv) " " (height cv))
   "255"])

; Following the header is the pixels data.
; - each pixel is represented by 3 integers: red, green, blue.
; - each value should be scaled from `0` to `255`.
; - each element should be separated from its neighbors by a space.
; - the file should end with an empty line.

(defn ppm-clamp ^double [^double v]
  (min 1. (max 0. v)))

(defn ppm-color [^"[D" col]
  (st/join
   " "
   (for [k (range 3)]
     (Math/round (+ 0.49 (* 255. (ppm-clamp (nc/entry col k))))))))

(defn ppm-data-row [row]
  (let [raw (st/join " " (map ppm-color row))]
    (loop [remaining raw
           rows []]
      (if (> (long ppm-max-line-length) (count remaining))
        (conj rows remaining)
        (let [^long split-index (st/last-index-of remaining " " ppm-max-line-length)]
          (recur (subs remaining (inc split-index))
                 (conj rows (subs remaining 0 split-index))))))))

(defn ppm-data [cv]
  (flatten (map ppm-data-row cv)))

; The PPM file starts with a header, immediately followed by the pixels data, and ends with an empty line.

(defn ppm-rows [cv]
  (conj
   (into
    (ppm-header cv)
    (ppm-data cv))
   ""))
