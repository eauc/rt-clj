(ns rt-clj.shapes.spheres-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.shapes.spheres :refer :all]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

(deftest spheres-test

  (testing "A ray intersects a sphere at two points"
    (let [ra (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          s (sphere)
          xs (sh/local-intersect s ra {})]
      (is (= 2
             (count xs)))
      (is (= 4.
             (:t (first xs))))
      (is (= 6.
             (:t (second xs))))))

  (testing "A ray intersects a sphere at a tangent"
    (let [ra (r/ray (t/point 0. 1. -5.) (t/vector 0. 0. 1.))
          s (sphere)
          xs (sh/local-intersect s ra {})]
      (is (= 2
             (count xs)))
      (is (= 5.
             (:t (first xs))))
      (is (= 5.
             (:t (second xs))))))

  (testing "A ray misses a sphere"
    (let [ra (r/ray (t/point 0. 2. -5.) (t/vector 0. 0. 1.))
          s (sphere)
          xs (sh/local-intersect s ra {})]
      (is (= 0
             (count xs)))))

  (testing "A ray originates inside a sphere"
    (let [ra (r/ray (t/point 0. 0. 0.) (t/vector 0. 0. 1.))
          s (sphere)
          xs (sh/local-intersect s ra {})]
      (is (= 2
             (count xs)))
      (is (= -1.
             (:t (first xs))))
      (is (= 1.
             (:t (second xs))))))

  (testing "A sphere is behind a ray"
    (let [ra (r/ray (t/point 0. 0. 5.) (t/vector 0. 0. 1.))
          s (sphere)
          xs (sh/local-intersect s ra {})]
      (is (= 2
             (count xs)))
      (is (= -6.
             (:t (first xs))))
      (is (= -4.
             (:t (second xs))))))

  (testing "The normal on a sphere at a point on the x axis"
    (is (t/eq? (t/vector 1. 0. 0.)
               (sh/local-normal (sphere) (t/point 1. 0. 0.) {}))))

  (testing "The normal on a sphere at a point on the y axis"
    (is (t/eq? (t/vector 0. 1. 0.)
               (sh/local-normal (sphere) (t/point 0. 1. 0.) {}))))

  (testing "The normal on a sphere at a point on the z axis"
    (is (t/eq? (t/vector 0. 0. 1.)
               (sh/local-normal (sphere) (t/point 0. 0. 1.) {}))))

  (testing "The normal on a sphere at a non-axial point"
    (let [p (/ (Math/sqrt 3.) 3.)]
      (is (t/eq? (t/vector p p p)
                 (sh/local-normal (sphere) (t/point p p p) {}))))))
