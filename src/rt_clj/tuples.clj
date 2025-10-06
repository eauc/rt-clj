; # Tuples

(ns rt-clj.tuples
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:refer-clojure :exclude [vector vector?])
  (:require [clojure.pprint :as pp]
            [fastmath.vector :as fv]))

; ## Creation

; We can create tuples, access their coordinates, and check whether they are points or vectors.

; Tuples are simple clojure vectors.

(defn tuple
  ([x y z w]
   (fv/vec4 x y z w))
  ([x y z]
   (tuple x y z 0.)))

(defn pprint [t]
  (print "T")
  (pp/pprint t)
  t)

(defn x ^double [v]
  (get v 0))

(defn y ^double [v]
  (get v 1))

(defn z ^double [v]
  (get v 2))

(defn w ^double [v]
  (get v 3))

(defn point? [tup]
  (= 1.0 (w tup)))

(defn vector? [tup]
  (= 0.0 (w tup)))

; We can also create point and vectors directly.

(defn point [x y z]
  (tuple x y z 1.0))

(defn vector [x y z]
  (tuple x y z 0.0))

(defn to-vector! [t]
  (assoc t 3 0.))

(def origin (point 0. 0. 0.))

(def zerov (vector 0. 0. 0.))

(defn rand-dv
  [^double radius]
  (- (* ^double (rand) 2. radius) radius))

(defn rand-vector
  [magnitude]
  (let [dx (rand-dv magnitude)
        dy (rand-dv magnitude)
        dz (rand-dv magnitude)]
    (vector dx dy dz)))

; ## Basic operations

; We need to define close equality for 2 floating-point scalars.

(def epsilon
  10e-6)

(def infinity
  10e300)

(defn close? [^double a ^double b]
  (> (double ^double epsilon) (Math/abs (- a b))))

; Then we need close equality of 2 tuples.

(defn eq? [a b]
  (fv/delta-eq a b epsilon))

; Tuples support basic addition & substraction.

(defn add [v w]
  (fv/add v w))

(defn sub [v w]
  (fv/sub v w))

; Vectors can be negated, multiplied and divided by scalars.

(defn neg [v]
  (fv/mult v -1.))

(defn mul [v s]
  (fv/mult v s))

(defn div [v s]
  (fv/div v s))

; We can get the dot and cross products of vectors.

(defn dot ^double [v w]
  (fv/dot v w))

(defn cross [v w]
  (vector (- (* (y v) (z w))
             (* (y w) (z v)))
          (- (* (z v) (x w))
             (* (z w) (x v)))
          (- (* (x v) (y w))
             (* (x w) (y v)))))

; We can get the magnitude of a vector.

(defn mag ^double [v]
  (fv/mag v))

; We can normalize a vector.

(defn norm [v]
  (fv/normalize v))

; ## Reflection

; Vectors can be reflected on a surface defined by a normal.

(defn reflect [in normal]
  (let [k (* 2 (dot in normal))]
    (sub in (mul normal k))))
