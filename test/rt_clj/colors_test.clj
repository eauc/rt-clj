(ns rt-clj.colors-test
  (:require [clojure.test :refer :all]
            [rt-clj.colors :refer :all]
            [rt-clj.tuples :as t]))

(deftest colors-test
  (testing "Colors are (red, green, blue) tuples"
    (let [col (color -0.5 0.4 1.7)]
      (is (= -0.5
             (red col)))
      (is (= 0.4
             (green col)))
      (is (= 1.7
             (blue col)))))

  (testing "Adding colors"
    (is (t/eq? (color 1.6 0.7 1.0)
               (add (color 0.9 0.6 0.75)
                    (color 0.7 0.1 0.25)))))

  (testing "Substracting colors"
    (is (t/eq? (color 0.2 0.5 0.5)
               (sub (color 0.9 0.6 0.75)
                    (color 0.7 0.1 0.25)))))

  (testing "Multiplying a color by a scalar"
    (is (t/eq? (color 0.4 0.6 0.8)
               (mul (color 0.2 0.3 0.4) 2.0))))

  (testing "Multiplying 2 colors"
    (is (t/eq? (color 0.9 0.2 0.04)
               (dot (color 1.0 0.2 0.4)
                    (color 0.9 1.0 0.1))))))
