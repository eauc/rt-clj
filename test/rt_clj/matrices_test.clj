(ns rt-clj.matrices-test
  (:require [clojure.test :refer :all]
            [rt-clj.matrices :refer :all]
            [rt-clj.tuples :as t]))

(deftest matrices-test
  (testing "Constructing and inspecting a 4x4 matrix"
    (let [m (matrix [[1. 2. 3. 4.]
                     [5.5 6.5 7.5 8.5]
                     [9. 10. 11. 12]
                     [13.5 14.5 15.5 16.5]])]
      (is (= 4
             (height m)))
      (is (= 4
             (width m)))
      (is (= 1.
             (get-at m 0 0)))
      (is (= 4.
             (get-at m 0 3)))
      (is (= 5.5
             (get-at m 1 0)))
      (is (= 7.5
             (get-at m 1 2)))
      (is (= 11.
             (get-at m 2 2)))
      (is (= 13.5
             (get-at m 3 0)))
      (is (= 15.5
             (get-at m 3 2)))))

  (testing "A 2x2 matrix ought to be representable"
    (let [m (matrix [[-3. 5.]
                     [1. -2.]])]
      (is (= 2
             (height m)))
      (is (= 2
             (width m)))
      (is (= -3.
             (get-at m 0 0)))
      (is (= 5.
             (get-at m 0 1)))
      (is (= 1.
             (get-at m 1 0)))
      (is (= -2.
             (get-at m 1 1)))))

  (testing "A 3x3 matrix ought to be representable"
    (let [m (matrix [[-3. 5. 0.]
                     [1. -2. -7.]
                     [0. 1. 1.]])]
      (is (= 3
             (height m)))
      (is (= 3
             (width m)))
      (is (= -3.
             (get-at m 0 0)))
      (is (= -2.
             (get-at m 1 1)))
      (is (= 1.
             (get-at m 2 2)))))

  (testing "Matrix equality with identical matrices"
    (is (eq? (matrix [[1 2 3 4]
                      [2 3 4 5]
                      [3 4 5 6]
                      [4 5 6 7]])
             (matrix [[1 2 3 4]
                      [2 3 4 5]
                      [3 4 5 6]
                      [4 5 6 7]]))))

  (testing "Matrix equality with different matrices"
    (is (not (eq? (matrix [[1 2 3 4]
                           [2 3 4 5]
                           [3 4 5 6]
                           [4 5 6 7]])
                  (matrix [[0 2 3 4]
                           [2 3 4 5]
                           [3 4 5 6]
                           [4 5 6 7]])))))

  (testing "Transposing a matrix"
    (is (eq? (matrix [[0 9 3 0]
                      [9 8 0 8]
                      [1 8 5 3]
                      [0 0 5 8]])
             (transpose (matrix [[0 9 1 0]
                                 [9 8 8 0]
                                 [3 0 5 5]
                                 [0 8 3 8]])))))

  (testing "Multiplying two matrices"
    (is (eq? (matrix [[24. 49. 98.  196.]
                      [31. 64. 128. 256.]
                      [38. 79. 158. 316.]
                      [45. 94. 188. 376.]])
             (mul (matrix [[1. 2. 3. 4.]
                           [2. 3. 4. 5.]
                           [3. 4. 5. 6.]
                           [4. 5. 6. 7.]])
                  (matrix [[0. 1. 2. 4.]
                           [1. 2. 4. 8.]
                           [2. 4. 8. 16.]
                           [4. 8. 16. 32.]])))))

  (testing "A matrix multiplied by a tuple"
    (is (t/eq? (t/tuple 18. 24. 33. 1.)
               (mul-t (matrix [[1. 2. 3. 4.]
                               [2. 4. 4. 2.]
                               [8. 6. 4. 1.]
                               [0. 0. 0. 1.]])
                      (t/tuple 1. 2. 3. 1.)))))

  (testing "Multiplying a matrix by the identity"
    (let [m (matrix [[0. 1. 2. 4.]
                     [1. 2. 4. 8.]
                     [2. 4. 8. 16.]
                     [4. 8. 16. 32.]])]
      (is (eq? m
               (mul m (id 4))))))

  (testing "Multiplying identity by a tuple"
    (let [a (t/tuple 1. 2. 3. 4.)]
      (is (t/eq? a
                 (mul-t (id 4) a)))))

  (testing "Calculating the determinant of a 2x2 matrix"
    (is (= 17.
           (det (matrix [[1. 5.]
                         [-3. 2.]])))))

  (testing "Calculating the determinant of a 3x3 matrix"
    (let [m (matrix [[1. 2. 6.]
                     [-5. 8. -4.]
                     [2. 6. 4.]])]
      (is (= -196.
             (det m)))))

  (testing "Calculating the determinant of a 4x4 matrix"
    (let [m (matrix [[-2. -8. 3. 5.]
                     [-3. 1. 7. 3.]
                     [1. 2. -9. 6.]
                     [-6. 7. 7. -9.]])]
      (is (= -4071.
             (det m)))))

  (testing "Testing an invertible matrix for invertibility"
    (let [m (matrix [[6 4 4 4]
                     [5 5 7 6]
                     [4 -9 3 -7]
                     [9 1 7 -6]])]
      (is (= -2120.
             (det m)))
      (is (invertible? m))))

  (testing "Testing an non-invertible matrix for invertibility"
    (let [m (matrix [[-4 2 -2 -3]
                     [9 6 2 6]
                     [0 -5 1 -5]
                     [0 0 0 0]])]
      (is (= 0.
             (det m)))
      (is (not (invertible? m)))))

  (testing "Calculating the inverse of a"
    (let [m (matrix [[-5. 2. 6. -8.]
                     [1. -5. 1. 8.]
                     [7. 7. -6. -7.]
                     [1. -3. 7. 4.]])]
      (is (= 532.
             (det m)))
      (is (eq? (matrix [[0.21805 0.45113 0.24060 -0.04511]
                        [-0.80827 -1.45677 -0.44361 0.52068]
                        [-0.07895 -0.22368 -0.05263 0.19737]
                        [-0.52256 -0.81391 -0.30075 0.30639]])
               (inverse m)))))

  (testing "The inverse of a non-invertible matrix is nil"
    (is (= nil
           (inverse (matrix [[-4 2 -2 -3]
                             [9 6 2 6]
                             [0 -5 1 -5]
                             [0 0 0 0]])))))

  (testing "Multiplying a product by its inverse"
    (let [a (matrix [[3. -9. 7. 3.]
                     [3. -8. 2. -9.]
                     [-4. 4. 4. 1.]
                     [-6. 5. -1. 1.]])
          b (matrix [[8. 2. 2. 2.]
                     [3. -1. 7. 0.]
                     [7. 0. 5. 4.]
                     [6. -2. 0. 5.]])]
      (is (eq? a
               (mul (mul a b) (inverse b))))))

  (testing "Multiplying a matrix by its inverse gives the identity"
    (let [a (matrix [[3. -9. 7. 3.]
                     [3. -8. 2. -9.]
                     [-4. 4. 4. 1.]
                     [-6. 5. -1. 1.]])]
      (is (eq? (matrix [[1. 0. 0. 0.]
                        [0. 1. 0. 0.]
                        [0. 0. 1. 0.]
                        [0. 0. 0. 1.]])
               (mul a (inverse a)))))))
  ;
  ; (testing "The inverse of identity is identity"
  ;   (is (eq? (id 4)
  ;            (inverse (id 4))))))
