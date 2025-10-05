(ns rt-clj.shapes.cylinders-test
  (:require [clojure.test :refer [deftest are testing]]
            [rt-clj.shapes.cylinders :refer :all]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

(deftest cylinders-test

  (testing "A ray misses a cylinder"
    (are [origin direction]
         (= []
            (sh/local-intersect (cylinder) (r/ray origin (t/norm direction)) {}))
      (t/point 1. 0. 0.) (t/vector 0. 1. 0.)
      (t/point 0. 0. 0.) (t/vector 0. 1. 0.)
      (t/point 0. 0. -5.) (t/vector 1. 1. 1.)))

  (testing "A ray strikes a cylinder"
    (are [origin direction t]
         (t/eq? t
                (mapv :t (sh/local-intersect (cylinder) (r/ray origin (t/norm direction)) {})))
      (t/point 1. 0. -5.) (t/vector 0. 0. 1.) [5. 5.]
      (t/point 0. 0. -5.) (t/vector 0. 0. 1.) [4. 6.]
      (t/point 0.5 0. -5.) (t/vector 0.1 1. 1.) [6.80798 7.08872]))

  (testing "Normal vector on a cylinder"
    (are [point normal]
         (t/eq? normal
                (sh/local-normal (cylinder) point {}))
      (t/point 1. 0. 0.) (t/vector 1. 0. 0.)
      (t/point 0. 5. -1.) (t/vector 0. 0. -1.)
      (t/point 0. -2. 1.) (t/vector 0. 0. 1.)
      (t/point -1. 1. 0.) (t/vector -1. 0. 0.)))

  (testing "Intersecting a constrained cylinder"
    (let [cyl (assoc (cylinder)
                     :minimum 1
                     :maximum 2)]
      (are [origin direction cnt]
           (= cnt
              (count (sh/local-intersect cyl (r/ray origin (t/norm direction)) {})))
        (t/point 0. 1.5 0.) (t/vector 0.1 1. 0.) 0
        (t/point 0. 3. -5.) (t/vector 0. 0. 1.) 0
        (t/point 0. 0. -5.) (t/vector 0. 0. 1.) 0
        (t/point 0. 2. -5.) (t/vector 0. 0. 1.) 0
        (t/point 0. 1. -5.) (t/vector 0. 0. 1.) 0
        (t/point 0. 1.5 -2.) (t/vector 0. 0. 1.) 2)))

  (testing "Intersecting the caps of a closed cylinder"
    (let [cyl (assoc (cylinder)
                     :minimum 1
                     :maximum 2
                     :closed? true)]
      (are [origin direction cnt]
           (= cnt
              (count (sh/local-intersect cyl (r/ray origin (t/norm direction)) {})))
        (t/point 0. 3. 0.) (t/vector 0. -1. 0.) 2
        (t/point 0. 3. -2.) (t/vector 0. -1. 2.) 2
        (t/point 0. 4. -2.) (t/vector 0. -1. 1.) 2
        (t/point 0. 0. -2.) (t/vector 0. 1. 2.) 2
        (t/point 0. -1. -2.) (t/vector 0. 1. 1.) 2)))

  (testing "The normal vector on a cylinder's end caps"
    (let [cyl (assoc (cylinder)
                     :minimum 1
                     :maximum 2
                     :closed? true)]
      (are [point normal]
           (t/eq? normal
                  (sh/local-normal cyl point {}))
        (t/point 0. 1. 0.) (t/vector 0. -1. 0.)
        (t/point 0.5 1. 0.) (t/vector 0. -1. 0.)
        (t/point 0. 1. 0.5) (t/vector 0. -1. 0.)
        (t/point 0. 2. 0.) (t/vector 0. 1. 0.)
        (t/point 0.5 2. 0.) (t/vector 0. 1. 0.)
        (t/point 0. 2. 0.5) (t/vector 0. 1. 0.)))))
