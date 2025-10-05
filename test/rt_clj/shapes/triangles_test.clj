(ns rt-clj.shapes.triangles-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.shapes.triangles :refer :all]
            [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.rays :as r]
            [rt-clj.tuples :as t]))

(deftest triangles-test

  (testing "Constructing a triangle"
    (let [p1 (t/point 0. 1. 0.)
          p2 (t/point -1. 0. 0.)
          p3 (t/point 1. 0. 0.)
          t (triangle p1 p2 p3)]
      (is (t/eq? p1
                 (:p1 t)))
      (is (t/eq? p2
                 (:p2 t)))
      (is (t/eq? p3
                 (:p3 t)))
      (is (t/eq? (t/vector -1. -1. 0.)
                 (:e1 t)))
      (is (t/eq? (t/vector 1. -1. 0.)
                 (:e2 t)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:normal t)))))

  (testing "Intersecting a ray parallel to the triangle"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))
          ray (r/ray (t/point 0. -1. -2.) (t/vector 0. 1. 0.))]
      (is (= []
             (sh/local-intersect tri ray {})))))

  (testing "A ray misses the p1-p3 edge"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))
          ray (r/ray (t/point 1. 1. -2.) (t/vector 0. 0. 1.))]
      (is (= []
             (sh/local-intersect tri ray {})))))

  (testing "A ray misses the p1-p2 edge"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))
          ray (r/ray (t/point -1. 1. -2.) (t/vector 0. 0. 1.))]
      (is (= []
             (sh/local-intersect tri ray {})))))

  (testing "A ray misses the p2-p3 edge"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))
          ray (r/ray (t/point 0. -1. -2.) (t/vector 0. 0. 1.))]
      (is (= []
             (sh/local-intersect tri ray {})))))

  (testing "A ray strikes a triangle"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))
          ray (r/ray (t/point 0. 0.5 -2.) (t/vector 0. 0. 1.))]
      (is (= [2.]
             (map :t (sh/local-intersect tri ray {}))))))

  (testing "An intersection with a smooth triangle stores u/v"
    (let [tri (triangle (t/point 0. 1. 0.) (t/point -1. 0. 0.) (t/point 1. 0. 0.))
          ray (r/ray (t/point -0.2 0.3 -2.) (t/vector 0. 0. 1.))
          [hit] (sh/local-intersect tri ray {})]
      (is (t/close? 0.45
                    (:u hit)))
      (is (t/close? 0.25
                    (:v hit)))))

  (testing "Finding the normal on a triangle"
    (let [tri (triangle (t/point 0. 1. 0.)
                        (t/point -1. 0. 0.)
                        (t/point 1. 0. 0.))]
      (is (t/eq? (:normal tri)
                 (sh/local-normal tri (t/point 0. 0.5 0.) {})))
      (is (t/eq? (:normal tri)
                 (sh/local-normal tri (t/point -0.5 0.75 0.) {})))
      (is (t/eq? (:normal tri)
                 (sh/local-normal tri (t/point 0.5 0.25 0.) {})))))

  (let [tri (smooth-triangle
             (t/point 0. 1. 0.)
             (t/point -1. 0. 0.)
             (t/point 1. 0. 0.)
             (t/vector 0. 1. 0.)
             (t/vector -1. 0. 0.)
             (t/vector 1. 0. 0.))]
    (testing "A smooth triangle uses u/v to interpolate the normal"
      (let [hit (i/intersection 1. tri 0.45  0.25)
            _ (println (t/x (sh/local-normal tri t/origin hit)))]
        (is (t/eq? (t/vector -0.554700 0.832050 0.)
                   (sh/local-normal tri t/origin hit)))))))
