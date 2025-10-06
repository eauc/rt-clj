; # Matrices

(ns rt-clj.matrices
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [clojure.pprint :as pp]
            [fastmath.matrix :as fm]
            [rt-clj.tuples :as t]))

; ## Creation

; Matrices are stored as simple vectors of rows.

(defn matrix [rows]
  (apply fm/rows->mat4x4 rows))

; (defn pprint ^"[[D" [^"[[D" m]
;   (print "M")
;   (pp/pprint (mapv #(into [] %) m))
;   m)
;
(defn height [m]
  (fm/nrow m))

(defn width [m]
  (fm/ncol m))

; We can inspect any element.

(defn get-at [m i j]
  (nth (fm/row m i) j))

; (defn set-at [^"[[D" m ^long i ^long j ^double v]
;   (aset ^"[D" (aget m i) j v))

; ## Equality

; Matrices very similar members are equals.

(defn eq? [a b]
  (every? true? (map t/eq? a b)))

; ## Transposition

; Invert the rows & cols of a matrix.

(defn transpose [m]
  (fm/transpose m))

; ## Multiplication

; We can multiply matrices.

; Element `[i,j]` is the dot product of A's row `[i]` & B's col `[j]`.

(defn mul-t [m t]
  (fm/mulv m t))

(defn mul [a b]
  (fm/mulm a b))

; ### Indentity matrix

; Multiplying any matrix or tuple by the identity leaves them unchanged.

(defn id [n]
  (matrix (for [i (range n)] (for [j (range n)] (if (= i j) 1. 0.)))))

; ## Inversion

; Inverting matrices starts with finding the determinant.

; ; ### Submatrices
;
; ; Finding the determinant of matrices larger than 2x2, involves finding the submatrices.
;
; ; A submatrice of A for element [i,j] is the matrix obtained by removing row i and col j of A.)
;
; (defn subm [^"[[D" m ^long l ^long c]
;   (let [h (dec (height m))
;         w (dec (width m))
;         r (make-array Double/TYPE h w)]
;     (dotimes [i h]
;       (dotimes [j w]
;         (set-at r i j (get-at m
;                               (if (< i l) i (inc i))
;                               (if (< j c) j (inc j))))))
;     r))
;
; ; ### Minors & Cofactors
; ;
; ; The minor of an element at row i and column j is the determinant of the submatrix at [i,j].
;
; (declare det)
;
; (defn minor ^double [m i j]
;   (det (subm m i j)))
;
; ; The cofactor of [i,j] is the minor of [i,j], negated if `i+j` is odd.
;
; (defn cofactor ^double [m ^long i ^long j]
;   (let [mi (minor m i j)]
;     (if (odd? (+ i j))
;       (- 0. mi)
;       mi)))

; ### Determinant

; For 2x2 matrices, the determinant is `a.d - b.c`

; For larger matrices:
; - we extract the first row.
; - we calculate the vector of the cofactors for each element of the first row.
; - the determinant is the dot product of the first row with the cofactors vector.

(defn det [m]
  (fm/det m))

; ### Inverse

; A matrix is invertible if the determinant is not 0.

(defn invertible? [m]
  (not (t/close? 0 (det m))))

; To calculate the inverse of a matrix:
; - we calculate the matrix of the cofactors.
; - we transpose those cofactors.
; - we divide each element by the determinant.

; (defn cofactors-t [m h w]
;   (let [r (make-array Double/TYPE w h)]
;     (dotimes [i h]
;       (dotimes [j w]
;         (set-at r j i (cofactor m i j))))
;     r))

(defn inverse [m]
  (fm/inverse m))
