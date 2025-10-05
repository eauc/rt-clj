(ns rt-clj.intersections-test
  (:require [clojure.test :refer [deftest is are testing]]
            [rt-clj.intersections :refer :all]
            [rt-clj.matrices :as ma]
            [rt-clj.materials :as m]
            [rt-clj.objects :as os]
            [rt-clj.rays :as r]
            [rt-clj.tuples :as t]
            [rt-clj.transformations :as tr]))

(deftest intersections-test
  (testing "An intersection encapsulates 't' and 'object'"
    (let [s {:object :test}
          i (intersection 3.5 s)]
      (is (= 3.5
             (:t i)))
      (is (= s
             (:object i)))))

  (testing "Aggregating intersections"
    (let [s {:object :test}
          i1 (intersection 1. s)
          i2 (intersection 2. s)
          xs (intersections i1 i2)]
      (is (= 2
             (count xs)))
      (is (= 1.
             (:t (first xs))))
      (is (= 2.
             (:t (second xs))))))

  (testing "Precomputing the state of an intersection"
    (let [ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          shape (os/sphere)
          hit (intersection 4 shape)
          p (prepare-hit hit ray [hit])]
      (is (t/eq? (t/point 0. 0. -1.00001)
                 (:point p)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:eyev p)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:normalv p)))))

  (testing "An intersection occurs on the outside"
    (let [ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          shape (os/sphere)
          hit (intersection 1. shape)
          p (prepare-hit hit ray [hit])]
      (is (= false
             (:inside? p)))))

  (testing "An intersection occurs on the inside"
    (let [ray (r/ray (t/point 0. 0. 0.) (t/vector 0. 0. 1.))
          shape (os/sphere)
          hit (intersection 1. shape)
          p (prepare-hit hit ray [hit])]
      (is (t/eq? (t/point 0. 0. 0.99999)
                 (:point p)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:eyev p)))
      (is (= true
             (:inside? p)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:normalv p)))))

  (testing "The hit should offset the point"
    (let [ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          shape (-> (os/sphere) (os/with-transform (tr/translation 0. 0. 1.)))
          h (intersection 5. shape)]
      (is (> (- (/ t/epsilon 2))
             (t/z (:point (prepare-hit h ray [h])))))))
  ;
  ; (testing "Precomputing the reflection vector"
  ;   (let [shape (os/plane)
  ;         ray (r/ray (t/point 0. 1. -1.) (t/vector 0. (- (/ (Math/sqrt 2.) 2)) (/ (Math/sqrt 2.) 2)))
  ;         int (intersection (Math/sqrt 2.) shape)]
  ;     (is (t/eq? (t/vector 0. (/ (Math/sqrt 2.) 2) (/ (Math/sqrt 2.) 2))
  ;                (:reflectv (prepare-hit int ray [int]))))))

  (testing "Finding n1 and n2 at various intersections"
    (let [A (os/sphere
             (assoc m/glass :refractive-index 1.5)
             (tr/scaling 2. 2. 2.))
          B (os/sphere
             (assoc m/glass :refractive-index 2.)
             (tr/translation 0. 0. -0.25))
          C (os/sphere
             (assoc m/glass :refractive-index 2.5)
             (tr/translation 0. 0. 0.25))
          ray (r/ray (t/point 0. 0. -4.) (t/vector 0. 0. 1.))
          xs [(intersection 2. A)
              (intersection 2.75 B)
              (intersection 3.25 C)
              (intersection 4.75 B)
              (intersection 5.25 C)
              (intersection 6. A)]]
      (are [k n] (= n
                    (:n (prepare-hit (nth xs k) ray xs)))
        0 [1.0 1.5]
        1 [1.5 2.0]
        2 [2.0 2.5]
        3 [2.5 2.5]
        4 [2.5 1.5]
        5 [1.5 1.0])))

  (testing "The under point is offset below the surface"
    (let [ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          shape (os/sphere
                 m/glass
                 (tr/translation 0. 0. 1.))
          inter (intersection 5. shape)
          xs [inter]
          hit (prepare-hit inter ray xs)
          _ (println (:under-point hit))]
      (is (< (/ t/epsilon 2.)
             (t/z (:under-point hit))))
      (is (< (t/z (:point hit))
             (t/z (:under-point hit))))))

  (testing "The hit, when all intersections have positive t"
    (let [s {:object :test}
          i1 (intersection 1. s)
          i2 (intersection 2. s)
          xs (intersections i2 i1)]
      (is (= i1
             (hit (sort-by :t xs))))))

  (testing "The hit, when some intersections have negative t"
    (let [s {:object :test}
          i1 (intersection -1. s)
          i2 (intersection 2. s)
          xs (intersections i2 i1)]
      (is (= i2
             (hit xs)))))

  (testing "The hit, when all intersections have negative t"
    (let [s {:object :test}
          i1 (intersection -1. s)
          i2 (intersection -2. s)
          xs (intersections i2 i1)]
      (is (= nil
             (hit xs)))))

  (testing "The hit is always the lowest non-negative intersection"
    (let [s {:object :test}
          i1 (intersection 5. s)
          i2 (intersection 7. s)
          i3 (intersection -3. s)
          i4 (intersection 2. s)
          xs (intersections i2 i1 i3 i4)]
      (is (= i4
             (hit (sort-by :t xs))))))

  (testing "The Schlick approximation under total internal reflection"
    (let [shape (os/sphere m/glass (ma/id 4))
          sqrt2_on2 (/ (Math/sqrt 2.) 2.)
          ray (r/ray (t/point 0. 0. sqrt2_on2) (t/vector 0. 1. 0.))
          xs [(intersection (- sqrt2_on2) shape)
              (intersection sqrt2_on2 shape)]
          hit (prepare-hit (nth xs 1) ray xs)]
      (is (= 1. (schlick hit)))))

  (testing "The Schlick approximation with a perpendicular viewing angle"
    (let [shape (os/sphere  m/glass (ma/id 4))
          ray (r/ray (t/point 0. 0. 0.) (t/vector 0. 1. 0.))
          xs [(intersection -1. shape)
              (intersection 1. shape)]
          hit (prepare-hit (nth xs 1) ray xs)]
      (is (t/close? 0.04
                    (schlick hit)))))

  (testing "The Schlick approximation with small angle and n2 > n1"
    (let [shape (os/sphere  m/glass (ma/id 4))
          ray (r/ray (t/point 0. 0.99 -2.) (t/vector 0. 0. 1.))
          xs [(intersection 1.8589 shape)]
          hit (prepare-hit (first xs) ray xs)]
      (is (t/close? 0.488730
                    (schlick hit))))))
