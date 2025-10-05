; # Tuples

(ns rt-clj.tuples
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:refer-clojure :exclude [vector vector?])
  (:require [clojure.pprint :as pp]))

; ## Creation

; We can create tuples, access their coordinates, and check whether they are points or vectors.

; Tuples are simple clojure vectors.

(defn tuple
  ([x y z w]
   (double-array [x y z w]))
  ([x y z]
   (tuple x y z 0.)))

(defn pprint ^"[D" [^"[D" t]
  (print "T")
  (pp/pprint (into [] t))
  t)

(defn x ^double [^"[D" v]
  (aget v 0))

(defn y ^double [^"[D" v]
  (aget v 1))

(defn z ^double [^"[D" v]
  (aget v 2))

(defn w ^double [^"[D" v]
  (aget v 3))

(defn point? [tup]
  (= 1.0 (w tup)))

(defn vector? [tup]
  (= 0.0 (w tup)))

; We can also create point and vectors directly.

(defn point [x y z]
  (tuple x y z 1.0))

(defn vector [x y z]
  (tuple x y z 0.0))

(defn to-vector! ^"[D" [^"[D" t]
  (aset t 3 0.)
  t)

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

(defn eq? [^"[D" a ^"[D" b]
  (every? true? (map close? a b)))

; Tuples support basic addition & substraction.

(defn add [^"[D" v ^"[D" w]
  (let [r (aclone v)]
    (dotimes [i 4]
      (aset r i (+ (aget v i) (aget w i))))
    r))

(defn sub [^"[D" v ^"[D" w]
  (let [r (aclone v)]
    (dotimes [i 4]
      (aset r i (- (aget v i) (aget w i))))
    r))

; Vectors can be negated, multiplied and divided by scalars.

(defn neg [^"[D" v]
  (let [r (aclone v)]
    (dotimes [i (alength v)]
      (aset r i (- 0. (aget v i))))
    r))

(defn mul [^"[D" v ^double s]
  (let [r (aclone v)]
    (dotimes [i 4]
      (aset r i (* (aget v i) s)))
    r))

(defn div [^"[D" v ^double s]
  (let [r (aclone v)]
    (dotimes [i 4]
      (aset r i (/ (aget v i) s)))
    r))

; We can get the dot and cross products of vectors.

(defn dot ^double [^"[D" v ^"[D" w]
  (loop [i 0
         sum 0.]
    (if (= 4 i)
      sum
      (recur (inc i)
             (+ sum (* (aget v i) (aget w i)))))))

(defn cross [^"[D" v ^"[D" w]
  (vector (- (* (y v) (z w))
             (* (y w) (z v)))
          (- (* (z v) (x w))
             (* (z w) (x v)))
          (- (* (x v) (y w))
             (* (x w) (y v)))))

; We can get the magnitude of a vector.

(defn mag ^double [v]
  (Math/sqrt (dot v v)))

; We can normalize a vector.

(defn norm [v]
  (div v (mag v)))

; ## Reflection

; Vectors can be reflected on a surface defined by a normal.

(defn reflect [^"[D" in ^"[D" normal]
  (let [r (aclone in)
        k (* 2 (dot in normal))]
    (dotimes [i (alength in)]
      (aset r i (- (aget in i) (* k (aget normal i)))))
    r))
