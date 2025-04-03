; # Matrices

(ns rt-clj.matrices
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.tuples :as t])
  (:require [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as nc]
            [uncomplicate.neanderthal.linalg :as nl]))

; ## Creation

; Matrices are stored as simple vectors of rows.

(defn matrix [m]
  (nn/dge (count m) (count (first m)) m {:layout :row}))

(defn height ^long [m]
  (nc/mrows m))

(defn width ^long [m]
  (nc/ncols m))

; We can inspect any element.

(defn get-at ^double [m ^long i ^long j]
  (nc/entry m i j))

(defn set-at [m ^long i ^long j ^double v]
  (nc/entry! m j i v))

; ## Equality

; Matrices very similar members are equals.

(defn eq? [a b]
  (->> (range (max (height b) (height a)))
       (map #(t/eq? (nc/row a %)
                    (nc/row b %)))
       (every? true?)))

; ## Transposition

; Invert the rows & cols of a matrix.

(defn transpose [m]
  (let [r (nc/copy m)]
    (nc/trans r)))

; ## Multiplication

; We can multiply matrices.

; Element `[i,j]` is the dot product of A's row `[i]` & B's col `[j]`.

(defn mul-t [m t]
  (nc/mv m t))

(defn mul [a b]
  (nc/mm a b))

; ### Indentity matrix

; Multiplying any matrix or tuple by the identity leaves them unchanged.

(defn id [^long n]
  (let [i (nn/dge (repeat n (repeat n 0.)))]
    (dotimes [k n]
      (nc/entry! i k k 1.))
    i))

; ## Inversion

; Inverting matrices starts with finding the determinant.

; ### Submatrices

; ### Determinant

; For 2x2 matrices, the determinant is `a.d - b.c`

; For larger matrices:
; - we extract the first row.
; - we calculate the vector of the cofactors for each element of the first row.
; - the determinant is the dot product of the first row with the cofactors vector.

(defn det [m]
  (try
   (nl/det (nl/trf m))
   (catch Exception _ 0.)))

; ### Inverse

; A matrix is invertible if the determinant is not 0.

(defn invertible? [m]
  (not (t/close? 0 (det m))))

; To calculate the inverse of a matrix:
; - we calculate the matrix of the cofactors.
; - we transpose those cofactors.
; - we divide each element by the determinant.

(defn inverse [m]
  (try
   (nl/tri (nl/trf m))
   (catch Exception _ nil)))
