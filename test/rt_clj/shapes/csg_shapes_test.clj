(ns rt-clj.shapes.csg-shapes-test
  (:require [clojure.test :refer [deftest is are testing]]
            [rt-clj.shapes.csg-shapes :refer :all]
            [rt-clj.bounds :as bd]
            [rt-clj.intersections :as i]
            [rt-clj.objects :as os]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]))

(deftest csg-shapes-test

  (testing "CSG is created with an operation and two shapes"
    (let [s1 (os/sphere)
          s2 (os/cube)
          shape (csg-shape :union s1 s2)]
      (is (= :union
             (:operation shape)))))

  (testing "Evaluating the rule for a CSG operation"
    (are [op lhit inl inr result]
         (= result
            (intersection-allowed op lhit inl inr))
      :union true true  true  false
      :union true true  false true
      :union true false true  false
      :union true false false true
      :union false true true  false
      :union false true false false
      :union false false true   true
      :union false false false  true
      :intersection true true true true
      :intersection true true false false
      :intersection true false true true
      :intersection true false false false
      :intersection false true true true
      :intersection false true false true
      :intersection false false true false
      :intersection false false false false
      :difference true true true false
      :difference true true false true
      :difference true false true false
      :difference true false false true
      :difference false true true true
      :difference false true false true
      :difference false false true false
      :difference false false false false))

  (testing "Filtering a list of intersections"
    (let [s1 (os/sphere)
          s2 (os/cube)
          shape (csg-shape :union s1 s2)
          ints [(i/intersection 1. (:left shape))
                (i/intersection 2. (:right shape))
                (i/intersection 3. (:left shape))
                (i/intersection 4. (:right shape))]]
      (is (= [(nth ints 0)
              (nth ints 3)]
             (filter-intersections shape ints)))))

  (testing "A ray misses a CSG object"
    (let [shape (csg-shape :union (os/sphere) (os/cube))
          ray (r/ray (t/point 0. 2. -5.) (t/vector 0. 0. 1.))]
      (is (= []
             (sh/local-intersect shape ray {:bounds bd/infinite})))))

  (testing "A ray hits a CSG object"
    (let [s1 (os/sphere)
          s2 (-> (os/sphere) (os/with-transform (tr/translation 0. 0. 0.5)))
          shape (csg-shape :union s1 s2)
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))]
      (is (= [(i/intersection 4. (:left shape))
              (i/intersection 6.5 (:right shape))]
             (sh/local-intersect shape ray {:bounds bd/infinite}))))))
