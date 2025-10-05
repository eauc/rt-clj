; # Planes

(ns rt-clj.shapes.planes
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.bounds :as bd]
            [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Intersections

; The normalized plane is `y=0`, with normal vector `n=[0,1,0]`.

; There are 4 cases to consider:
; - the ray is parallel to the plane: no hit.
; - the ray is coplanar to the plane: no hit (planes are infinitely thins).
; - the ray origin is above the plane.
; - the ray origin is below the plane.

(defn- local-intersect [{:keys [origin direction]}, object]
  (if (t/close? 0. (t/y direction))
    []
    (let [t (- (/ (t/y origin) (t/y direction)))]
      [(i/intersection t object)])))

; ## Normal

; The local-normal of plane is always `[0 1 0]`.

(defn- local-normal []
  (t/vector 0. 1. 0.))

; ## Creation

; Planes are records implementing Shape protocol.

(defrecord Plane []
  sh/Shape
  (local-bounds [_]
    (bd/bounds
     (t/point (- (double t/infinity)) (- (double t/epsilon)) (- (double t/infinity)))
     (t/point t/infinity t/epsilon t/infinity)))
  (prepare-bounds [shape]
    shape)
  (prepare-material [shape _]
    shape)
  (prepare-transform [shape _ _]
    shape)
  (includes? [_ _]
    false)
  (local-intersect [_ ray object]
    (local-intersect ray object))
  (local-normal [_ _ _]
    (local-normal)))

(defn plane []
  (->Plane))
