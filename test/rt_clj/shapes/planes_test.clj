(ns rt-clj.shapes.planes-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.shapes.planes :refer :all]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

(deftest planes-test

  (testing "Intersect with a ray parallel to the plane"
    (is (= []
           (sh/local-intersect
            (plane)
            (r/ray (t/point 0. 10. 0.) (t/vector 0. 0. 1.))
            {}))))

  (testing "Intersect with a coplanar plane"
    (is (= []
           (sh/local-intersect
            (plane)
            (r/ray (t/point 0. 0. 0.) (t/vector 0. 0. 1.))
            {}))))

  (testing "A ray intersecting a plane from above"
    (let [p (plane)
          xs (sh/local-intersect
              p
              (r/ray (t/point 0. 1. 0.) (t/vector 0. -1. 0.))
              {})]
      (is (= 1
             (count xs)))
      (is (= 1.
             (:t (first xs))))))

  (testing "A ray intersecting a plane from below"
    (let [p (plane)
          xs (sh/local-intersect
              p
              (r/ray (t/point 0. -1. 0.) (t/vector 0. 1. 0.))
              {})]
      (is (= 1
             (count xs)))
      (is (= 1.
             (:t (first xs))))))

  (testing "The normal of a plane is constant everywhere"
    (is (t/eq? (t/vector 0. 1. 0.)
               (sh/local-normal (plane) (t/point 0. 0. 0.) {})))
    (is (t/eq? (t/vector 0. 1. 0.)
               (sh/local-normal (plane) (t/point 10. 0. -10.) {})))
    (is (t/eq? (t/vector 0. 1. 0.)
               (sh/local-normal (plane) (t/point -5. 0. 150.) {})))))
