; # Colors

(ns rt-clj.colors
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [fastmath.vector :as fv]
            [rt-clj.tuples :as t]))

; ## Creation

; Colors are (red, green, blue) tuples.

(def color fv/vec3)

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

(defn dot [v w]
  (fv/emult v w))

(defn avg [colors]
  (fv/average-vectors colors))
