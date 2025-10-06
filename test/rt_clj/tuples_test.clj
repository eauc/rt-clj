(ns rt-clj.tuples-test
  (:import java.lang.Math)
  (:refer-clojure :exclude [vector vector?])
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.tuples :refer :all]))

(deftest tuples-test
  (testing "A tuple with w=1.0 is a point"
    (let [tup (tuple 4.1 -4.2 3.1 1.0)]
      (is (= 4.1
             (x tup)))
      (is (= -4.2
             (y tup)))
      (is (= 3.1
             (z tup)))
      (is (= 1.0
             (w tup)))
      (is (= true
             (point? tup)) "is point")
      (is (= false
             (vector? tup)) "is not vector")))

  (testing "A tuple with w=0.0 is a vector"
    (let [tup (tuple 4.1 -4.2 3.1 0.0)]
      (is (= 4.1
             (x tup)))
      (is (= -4.2
             (y tup)))
      (is (= 3.1
             (z tup)))
      (is (= 0.0
             (w tup)))
      (is (= false
             (point? tup)) "is not point")
      (is (= true
             (vector? tup)) "is vector")))

  (testing "'point' create a tuple with w=1.0"
    (is (eq? (tuple 4.1 -4.2 3.1 1.0)
             (point 4.1 -4.2 3.1))))

  (testing "'vector' create a tuple with w=0.0"
    (is (eq? (tuple 4.1 -4.2 3.1 0.0)
             (vector 4.1 -4.2 3.1))))

  (testing "Adding a point and a vector gives a point"
    (is (eq? (point 1 1 6)
             (add (point 3 -2 5)
                  (vector -2 3 1)))))

  (testing "Adding 2 vectors gives a vector"
    (is (eq? (vector 1 1 6)
             (add (vector 3 -2 5)
                  (vector -2 3 1)))))

  (testing "Adding 2 points does not make sense"
    (is (eq? (tuple 1 1 6 2.0)
             (add (point 3 -2 5)
                  (point -2 3 1)))))

  (testing "Substracting 2 points gives a vector"
    (is (eq? (vector -2 -4 -6)
             (sub (point 3 2 1)
                  (point 5 6 7)))))

  (testing "Substracting a vector from a point gives a point"
    (is (eq? (point -2 -4 -6)
             (sub (point 3 2 1)
                  (vector 5 6 7)))))

  (testing "Substracting 2 vectors gives a vector"
    (is (eq? (vector -2 -4 -6)
             (sub (vector 3 2 1)
                  (vector 5 6 7)))))

  (testing "Substracting a point from a vector makes no sense"
    (is (eq? (tuple -2 -4 -6 -1.0)
             (sub (vector 3 2 1)
                  (point 5 6 7)))))

  (testing "Negating a vector gives a vector"
    (is (eq? (vector -3 -2 -1)
             (neg (vector 3 2 1)))))

  (testing "Negating a points makes no sense"
    (is (eq? (tuple -3 -2 -1 -1.0)
             (neg (point 3 2 1)))))

  (testing "Multiplying a vector by a scalar gives a vector"
    (is (eq? (vector 3.5 -7.0 10.5)
             (mul (vector 1 -2 3) 3.5))))

  (testing "Multiplying a point by a scalar does not make sense"
    (is (eq? (tuple 3.5 -7.0 10.5 3.5)
             (mul (point 1 -2 3) 3.5))))

  (testing "Dividing a vector by a scalar gives a vector"
    (is (eq? (vector 0.5 -1.0 1.5)
             (div (vector 1.0 -2.0 3.0) 2))))

  (testing "Dividing a point by a scalar does not make sense"
    (is (eq? (tuple 0.5 -1.0 1.5 0.5)
             (div (point 1.0 -2.0 3.0) 2))))

  (testing "Magnitude"
    (is (= 1.0
           (mag (vector 1.0 0.0 0.0))))
    (is (= 1.0
           (mag (vector 0.0 1.0 0.0))))
    (is (= 1.0
           (mag (vector 0.0 0.0 1.0))))
    (is (= (Math/sqrt 14.0)
           (mag (vector 1.0 2.0 3.0))))
    (is (= (Math/sqrt 14.0)
           (mag (vector -1.0 -2.0 -3.0)))))

  (testing "Normalize"
    (is (eq? (vector 1.0 0.0 0.0)
             (norm (vector 4.0 0.0 0.0))))
    (is (eq? (vector (/ 1.0 (Math/sqrt 14)) (/ 2.0 (Math/sqrt 14)) (/ 3.0 (Math/sqrt 14)))
             (norm (vector 1.0 2.0 3.0)))))

  (testing "Dot product"
    (is (= 20.0
           (dot (vector 1.0 2.0 3.0)
                (vector 2.0 3.0 4.0)))))

  (testing "Cross product"
    (is (eq? (vector -1.0 2.0 -1.0)
             (cross (vector 1.0 2.0 3.0)
                    (vector 2.0 3.0 4.0))))
    (is (eq? (vector 1.0 -2.0 1.0)
             (cross (vector 2.0 3.0 4.0)
                    (vector 1.0 2.0 3.0)))))

  (testing "Reflecting a vector approaching at 45°"
    (is (eq? (vector 1. 1. 0.)
             (reflect (vector 1. -1. 0.) (vector 0. 1. 0.)))))

  (testing "Reflecting a vector off a slanted surface"
    (let [d (/ (Math/sqrt 2.) 2.)]
      (is (eq? (vector 1. 0. 0.)
               (reflect (vector 0. -1. 0.) (vector d d 0.)))))))
