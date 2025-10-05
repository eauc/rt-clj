; # Matrices

(ns rt-clj.matrices
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [clojure.pprint :as pp]
            [rt-clj.tuples :as t]))

; ## Creation

; Matrices are stored as simple vectors of rows.

(defn matrix ^"[[D" [m]
  (let [h (count m)
        w (count (first m))
        r (make-array Double/TYPE h w)]
    (dotimes [i h]
      (dotimes [j w]
        (let [v ((m i) j)]
          (aset r i j v))))
    r))

(defn pprint ^"[[D" [^"[[D" m]
  (print "M")
  (pp/pprint (mapv #(into [] %) m))
  m)

(defn height ^long [^"[[D" m]
  (alength m))

(defn width ^long [^"[[D" m]
  (let [^"[D" r (aget m 0)]
    (alength r)))

; We can inspect any element.

(defn get-at ^double [^"[[D" m ^long i ^long j]
  (aget ^"[D" (aget m i) j))

(defn set-at [^"[[D" m ^long i ^long j ^double v]
  (aset ^"[D" (aget m i) j v))

; ## Equality

; Matrices very similar members are equals.

(defn eq? [a b]
  (every? true? (map t/eq? a b)))

; ## Transposition

; Invert the rows & cols of a matrix.

(defn transpose [^"[[D" m]
  (let [r (make-array Double/TYPE (width m) (height m))]
    (dotimes [j (height m)]
      (dotimes [i (width m)]
        (set-at r i j (get-at m j i))))
    r))

; ## Multiplication

; We can multiply matrices.

; Element `[i,j]` is the dot product of A's row `[i]` & B's col `[j]`.

(defn mul-t [^"[[D" m ^"[D" t]
  (let [^"[D" r (aclone t)]
    (dotimes [i (alength t)]
      (let [c (t/dot (aget m i) t)]
        (aset r i c)))
    r))

(defn mul [^"[[D" a ^"[[D" b]
  (let [h (height a)
        w (width b)
        it (height b)
        r (make-array Double/TYPE h w)]
    (dotimes [i h]
      (dotimes [j w]
        (loop [k 0
               sum 0.]
          (if (= k it)
            (set-at r i j sum)
            (recur (inc k) (+ sum (* (get-at a i k) (get-at b k j))))))))
    r))

; ### Indentity matrix

; Multiplying any matrix or tuple by the identity leaves them unchanged.

(defn id [^long n]
  (let [r (make-array Double/TYPE n n)]
    (dotimes [i n]
      (dotimes [j n]
        (set-at r i j (if (= i j) 1. 0.))))
    r))

; ## Inversion

; Inverting matrices starts with finding the determinant.

; ### Submatrices

; Finding the determinant of matrices larger than 2x2, involves finding the submatrices.

; A submatrice of A for element [i,j] is the matrix obtained by removing row i and col j of A.)

(defn subm [^"[[D" m ^long l ^long c]
  (let [h (dec (height m))
        w (dec (width m))
        r (make-array Double/TYPE h w)]
    (dotimes [i h]
      (dotimes [j w]
        (set-at r i j (get-at m
                              (if (< i l) i (inc i))
                              (if (< j c) j (inc j))))))
    r))

; ### Minors & Cofactors
;
; The minor of an element at row i and column j is the determinant of the submatrix at [i,j].

(declare det)

(defn minor ^double [m i j]
  (det (subm m i j)))

; The cofactor of [i,j] is the minor of [i,j], negated if `i+j` is odd.

(defn cofactor ^double [m ^long i ^long j]
  (let [mi (minor m i j)]
    (if (odd? (+ i j))
      (- 0. mi)
      mi)))

; ### Determinant

; For 2x2 matrices, the determinant is `a.d - b.c`

; For larger matrices:
; - we extract the first row.
; - we calculate the vector of the cofactors for each element of the first row.
; - the determinant is the dot product of the first row with the cofactors vector.

(defn det [^"[[D" m]
  (let [w (width m)]
    (if (and (= 2 (height m))
             (= 2 w))
      (- (* (get-at m 0 0)
            (get-at m 1 1))
         (* (get-at m 0 1)
            (get-at m 1 0)))
      (loop [k 0
             d 0.]
        (if (= k w)
          d
          (recur (+ k 1) (+ d (* (get-at m 0 k) (cofactor m 0 k)))))))))

; ### Inverse

; A matrix is invertible if the determinant is not 0.

(defn invertible? [m]
  (not (t/close? 0 (det m))))

; To calculate the inverse of a matrix:
; - we calculate the matrix of the cofactors.
; - we transpose those cofactors.
; - we divide each element by the determinant.

(defn cofactors-t [m h w]
  (let [r (make-array Double/TYPE w h)]
    (dotimes [i h]
      (dotimes [j w]
        (set-at r j i (cofactor m i j))))
    r))

(defn inverse [m]
  (let [h (height m)
        w (width m)
        cfs (cofactors-t m h w)
        ^double d (loop [k 0 sum 0.]
                    (if (= k w)
                      sum
                      (recur (inc k) (+ sum (* (get-at cfs k 0) (get-at m 0 k))))))]
    (if (t/close? 0 d)
      (throw (ex-info "Cannot inverse matrix with det=0," {:d d :m m}))
      (do
        (dotimes [i w]
          (dotimes [j h]
            (set-at cfs i j (/ (get-at cfs i j) d))))
        cfs))))
