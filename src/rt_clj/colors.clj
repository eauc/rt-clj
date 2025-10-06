; # Colors

(ns rt-clj.colors
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.tuples :as t]))

; ## Creation

; Colors are (red, green, blue) tuples.

(def color t/tuple)

(def black (color 0. 0. 0.))

(def white (color 1. 1. 1.))

(def red t/x)

(def green t/y)

(def blue t/z)

; ## Operations

; Colors support addition, substraction and multiplication by a scalar.

(def add t/add)

(def sub t/sub)

(def mul t/mul)

(defn dot [^"[D" v ^"[D" w]
  (let [r (aclone v)]
    (dotimes [i 3]
      (aset r i (* (aget v i) (aget w i))))
    r))

(defn avg ^"[D" [colors]
  (let [n (count colors)
        r (aclone ^"[D" (first colors))]
    (dotimes [i (dec n)]
      (dotimes [j 3]
        (aset r j (+ (aget r j) (aget ^"[D" (nth colors (inc i)) j)))))
    (dotimes [j 3]
      (aset r j (/ (aget r j) n)))
    r))
