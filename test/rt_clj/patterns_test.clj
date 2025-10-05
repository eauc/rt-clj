(ns rt-clj.patterns-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.pattern-protocol :refer :all]
            [rt-clj.patterns :refer :all]
            [rt-clj.colors :as c]
            [rt-clj.objects :as os]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]))

(deftest patterns-test

  (testing "A pattern with an object transformation"
    (let [shape (-> (os/sphere) (os/with-transform (tr/scaling 2. 2. 2.)))
          pattern (test-pattern)]
      (is (t/eq? (c/color 1. 1.5 2.)
                 (pattern-at-shape pattern shape (t/point 2. 3. 4.))))))

  (testing "A pattern with a pattern transformation"
    (let [shape (os/sphere)
          pattern (test-pattern (tr/scaling 2. 2. 2.))]
      (is (t/eq? (c/color 1. 1.5 2.)
                 (pattern-at-shape pattern shape (t/point 2. 3. 4.))))))

  (testing "A pattern with both an object and a pattern transformation"
    (let [shape (-> (os/sphere) (os/with-transform (tr/scaling 2. 2. 2.)))
          pattern (test-pattern (tr/translation 0.5 1. 1.5))]
      (is (t/eq? (c/color 0.75 0.5 0.25)
                 (pattern-at-shape pattern shape (t/point 2.5 3. 3.5))))))

  (testing "A stripe pattern is constant in y"
    (let [pattern (stripes c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 1. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 2. 0.))))))

  (testing "A stripe pattern is constant in z"
    (let [pattern (stripes c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 1.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 2.))))))

  (testing "A stripe pattern alternates in x"
    (let [pattern (stripes c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0.9 0. 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 1.0 0. 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point -0.1 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point -1. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point -1.1 0. 0.))))))

  (testing "Stripes with an object transformation"
    (let [object (-> (os/sphere) (os/with-transform (tr/scaling 2. 2. 2.)))
          pattern (stripes c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at-shape pattern object (t/point 1.5 0. 0.))))))

  (testing "Stripes with a pattern transformation"
    (let [object (os/sphere)
          pattern (stripes c/white c/black (tr/scaling 2. 2. 2.))]
      (is (t/eq? c/white
                 (pattern-at-shape pattern object (t/point 1.5 0. 0.))))))

  (testing "Stripes with both an object and a pattern transformation"
    (let [object (-> (os/sphere) (os/with-transform (tr/scaling 2. 2. 2.)))
          pattern (stripes c/white c/black (tr/translation 0.5 0. 0.))]
      (is (t/eq? c/white
                 (pattern-at-shape pattern object (t/point 2.5 0. 0.))))))

  (testing "A gradient linearly interpolates between colors"
    (let [pattern (gradient c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? (c/color 0.75 0.75 0.75)
                 (pattern-at pattern (t/point 0.25 0. 0.))))
      (is (t/eq? (c/color 0.5 0.5 0.5)
                 (pattern-at pattern (t/point 0.5 0. 0.))))
      (is (t/eq? (c/color 0.25 0.25 0.25)
                 (pattern-at pattern (t/point 0.75 0. 0.))))))

  (testing "A ring should extend in both x and z"
    (let [pattern (rings c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 1. 0. 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 0. 0. 1.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 0.708 0. 0.708))))))

  (testing "Checkers should repeat in x"
    (let [pattern (checker c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0.99 0. 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 1.01 0. 0.))))))

  (testing "Checkers should repeat in y"
    (let [pattern (checker c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0.99 0.))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 0. 1.01 0.))))))

  (testing "Checkers should repeat in z"
    (let [pattern (checker c/white c/black)]
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.))))
      (is (t/eq? c/white
                 (pattern-at pattern (t/point 0. 0. 0.99))))
      (is (t/eq? c/black
                 (pattern-at pattern (t/point 0. 0. 1.01)))))))
