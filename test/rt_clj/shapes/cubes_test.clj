(ns rt-clj.shapes.cubes-test
  (:require [clojure.test :refer [deftest are testing]]
            [rt-clj.shapes.cubes :refer :all]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.rays :as r]
            [rt-clj.tuples :as t]))

(deftest patterns-test

  (testing "A ray intersects a cube"
    (are [origin direction t]
         (= t
            (mapv :t (sh/local-intersect (cube) (r/ray origin direction) {})))
      (t/point 5. 0.5 0.) (t/vector -1. 0. 0.) [4. 6.]
      (t/point -5. 0.5 0.) (t/vector 1. 0. 0.) [4. 6.]
      (t/point 0.5 5. 0.) (t/vector 0. -1. 0.) [4. 6.]
      (t/point 0.5 -5. 0.) (t/vector 0. 1. 0.) [4. 6.]
      (t/point 0.5 0. 5.) (t/vector 0. 0. -1.) [4. 6.]
      (t/point 0.5 0. -5.) (t/vector 0. 0. 1.) [4. 6.]
      (t/point 0. 0.5 0.) (t/vector 0. 0. 1.) [-1. 1.]))

  (testing "A ray misses a cube"
    (are [origin direction]
         (= []
            (sh/local-intersect (cube) (r/ray origin direction) {}))
      (t/point -2. 0. 0.) (t/vector 0.2673 0.5345 0.8018)
      (t/point 0. -2. 0.) (t/vector 0.8018 0.2673 0.5345)
      (t/point 0. 0. -2.) (t/vector 0.5345 0.8018 0.2673)
      (t/point 2. 0. 2.) (t/vector 0. 0. -1.)
      (t/point 0. 2. 2.) (t/vector 0. -1. 0.)
      (t/point 2. 2. 0.) (t/vector -1. 0. 0.)))

  (testing "The normal on the surface of a cube"
    (are [point normal]
         (t/eq? normal
                (sh/local-normal (cube) point {}))
      (t/point 1. 0.5 -0.8) (t/vector 1. 0. 0.)
      (t/point -1. -0.2 0.9) (t/vector -1. 0. 0.)
      (t/point -0.4 1. -0.1) (t/vector 0. 1. 0.)
      (t/point 0.3 -1. -0.7) (t/vector 0. -1. 0.)
      (t/point -0.6 0.3 1.) (t/vector 0. 0. 1.)
      (t/point 0.4 0.4 -1.) (t/vector 0. 0. -1.)
      (t/point 1. 1. 1.) (t/vector 1. 0. 0.)
      (t/point -1. -1. -1.) (t/vector -1. 0. 0.))))
