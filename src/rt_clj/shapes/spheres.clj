; # Spheres

(ns rt-clj.shapes.spheres
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Intersections
;
; A ray aways interesects a sphere at 2 points, even when tangent, or it totally misses the sphere.
; - when a ray is tangent to a sphere, both distances are equals.
; - when the ray originates inside a sphere, one of the distance is negative.
; - when the sphere is behing the ray's origin, both distances are negatives.

; The first intersection distance is always the smallest.

(defn- local-intersect [{:keys [^"[D" origin ^"[D" direction]} object]
  (let [s->ra (aclone origin)
        _ (aset s->ra 3 0.)
        two-a (* (t/dot direction direction) 2.)
        b (* 2. (t/dot direction s->ra))
        c (- (t/dot s->ra s->ra) 1.)
        discriminant (- (* b b) (* 2 two-a c))]
    (if (< discriminant 0)
      []
      (let [s-d (Math/sqrt discriminant)
            t1 (/ (- 0. s-d b) two-a)
            t2 (/ (- s-d b) two-a)
            i1 (i/intersection t1 object)
            i2 (i/intersection t2 object)]
        [i1 i2]))))

; ## Normal

; Normal is easy to calculate since the sphere is always centered at the origin.

(defn- local-normal [^"[D" object-p]
  (let [n (aclone object-p)]
    (aset n 3 0.)
    n))

; ## Creation

; Spheres are record implementing Shape protocol.

(defrecord Sphere []
  sh/Shape
  (local-bounds [_]
    {:min (t/point -1. -1. -1)
     :max (t/point 1. 1. 1.)})
  (local-intersect [_ ray object]
    (local-intersect ray object))
  (local-normal [_ point _]
    (local-normal point)))

(defn sphere []
  (->Sphere))
