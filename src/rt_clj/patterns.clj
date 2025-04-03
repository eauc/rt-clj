; # Patterns

(ns rt-clj.patterns
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [rt-clj.colors :as col]
            [rt-clj.matrices :as mat]
            [rt-clj.pattern-protocol :as pt]
            [rt-clj.tuples :as t]
            [uncomplicate.neanderthal.core :as nc]))

; ## Test Pattern

; We need a test pattern to test our protocol.

; Let's define a pattern that just returns the point's coordinates as a color.

(defrecord TestPattern [transform inverse-t]
  pt/Pattern
  (pattern-at [_ p]
    (col/color (t/x p) (t/y p) (t/z p))))

(defn test-pattern
  ([transform]
   (map->TestPattern
    {:transform transform
     :inverse-t (mat/inverse transform)}))
  ([]
   (test-pattern (mat/id 4))))

; ## Stripes
;
; [[file:../samples/patterns_stripes_example.png]]
;
; As the x coordinate changes, the Stripes pattern alternates between the two colors.
; The other two dimensions, y and z , have no effect on it.

(defrecord Stripes [a b transform inverse-t]
  pt/Pattern
  (pattern-at [{:keys [a b]} p]
    (let [x (t/x p)]
      (if (<= 0. x)
        (if (= 1 (mod (int x) 2)) b a)
        (if (= 0 (mod (- (int x)) 2)) b a)))))

(defn stripes
  ([a b transform]
   (map->Stripes
    {:a a :b b
     :transform transform
     :inverse-t (mat/inverse transform)}))
  ([a b]
   (stripes a b (mat/id 4))))

; ## Gradient

; [[file:../samples/patterns_gradient_example.png]]

; The Gradient pattern returns a blend of the two colors, linearly interpolating from one to the other as the x coordinate changes.

(defrecord Gradient [a b-a transform inverse-t]
  pt/Pattern
  (pattern-at [{:keys [a b-a]} p]
    (let [x (t/x p)
          x' (- x (Math/floor x))]
      (nc/axpy x' b-a a))))

(defn gradient
  ([a b transform]
   (map->Gradient
    {:a a :b-a (col/sub b a)
     :transform transform
     :inverse-t (mat/inverse transform)}))
  ([a b]
   (gradient a b (mat/id 4))))

; ## Rings

; [[file:../samples/patterns_rings_example.png]]

; A ring pattern depends on two dimensions, x and z, to decide which color to return.
; It tests the distance of the point in both x and z, which results in this pattern of concentric circles.

(defrecord Rings [a b transform inverse-t]
  pt/Pattern
  (pattern-at [{:keys [a b]} p]
    (let [x (t/x p)
          z (t/z p)
          r (Math/floor (Math/sqrt (+ (* x x) (* z z))))]
      (if (= 0.0 (mod r 2))
        a b))))

(defn rings
  ([a b transform]
   (map->Rings
    {:a a :b b
     :transform transform
     :inverse-t (mat/inverse transform)}))
  ([a b]
   (rings a b (mat/id 4))))

; ## Checker

; [[file:../samples/patterns_checker_example.png]]

; Checker is a pattern of alternating cubes, where two cubes of the same color are never adjacent.

(defrecord Checker [a b transform inverse-t]
  pt/Pattern
  (pattern-at [{:keys [a b]} p]
    (let [d (+ (Math/floor (t/x p))
               (Math/floor (t/y p))
               (Math/floor (t/z p)))]
      (if (= 0. (mod d 2))
        a b))))

(defn checker
  ([a b transform]
   (map->Checker
    {:a a :b b
     :transform transform
     :inverse-t (mat/inverse transform)}))
  ([a b]
   (checker a b (mat/id 4))))
