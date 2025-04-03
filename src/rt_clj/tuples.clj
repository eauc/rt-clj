; # Tuples

(ns rt-clj.tuples
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:refer-clojure :exclude [vector vector?])
  (:require [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as nc]))

; ## Creation

; We can create tuples, access their coordinates, and check whether they are points or vectors.

; Tuples are simple clojure vectors.

(defn tuple
  ([x y z w]
   (nn/dv x y z w))
  ([x y z]
   (tuple x y z 0.)))

(defn x ^double [v]
  (nc/entry v 0))

(defn y ^double [v]
  (nc/entry v 1))

(defn z ^double [v]
  (nc/entry v 2))

(defn w ^double [v]
  (nc/entry v 3))

(defn point? [tup]
  (= 1.0 (w tup)))

(defn vector? [tup]
  (= 0.0 (w tup)))

; We can also create point and vectors directly.

(defn point [x y z]
  (tuple x y z 1.0))

(defn vector [x y z]
  (tuple x y z 0.0))

(def origin (point 0. 0. 0.))

(def zerov (vector 0. 0. 0.))

; ## Basic operations

; We need to define close equality for 2 floating-point scalars.

(def epsilon
  10e-6)

(def infinity
  10e300)

(defn close? [^double a ^double b]
  (> (double epsilon) (Math/abs (- a b))))

; Then we need close equality of 2 tuples.

(defn eq? [a b]
  (every? true? (map close? a b)))

; Tuples support basic addition & substraction.

(defn add [v w]
  (nc/xpy v w))

(defn sub [v w]
  (nc/axpy -1.0 w v))

; Vectors can be negated, multiplied and divided by scalars.

(defn neg [v]
  (nc/ax -1.0 v))

(defn mul [v ^double s]
  (nc/ax s v))

(defn div [v ^double s]
  (nc/ax (/ 1.0 s) v))

; We can get the dot and cross products of vectors.

(defn dot ^double [v w]
  (nc/dot v w))

(defn cross [v w]
  (vector (- (* (y v) (z w))
             (* (y w) (z v)))
          (- (* (z v) (x w))
             (* (z w) (x v)))
          (- (* (x v) (y w))
             (* (x w) (y v)))))

; We can get the magnitude of a vector.

(defn mag ^double [v]
  (nc/nrm2 v))

; We can normalize a vector.

(defn norm [v]
  (div v (mag v)))

; ## Reflection

; Vectors can be reflected on a surface defined by a normal.

(defn reflect [in normal]
  (let [k (- 0. (* 2 (dot in normal)))]
    (nc/axpy k normal in)))
