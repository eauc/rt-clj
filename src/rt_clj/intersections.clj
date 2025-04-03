; # Intersections

(ns rt-clj.intersections
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.rays :as r]
            [rt-clj.shapes :as sh]
            [rt-clj.tuples :as t]
            [uncomplicate.neanderthal.core :as nc]))

; ## Creation

; Intersections store the object hit and the distance from origin on the ray.

(defn intersection [t object]
  {:t t
   :object object})

(def intersections vector)

; ## Hit

; We can find a hit in a list of intersections. It's always the lowest non-negative intersection.

(defn hit [is]
  (first (filter #(< 0. ^double (:t %)) is)))

; ## Refraction

; We determine the refractive indices on each side of the intersection.

(defn refractive-indices
  [hit is]
  (loop [[current & rest] is
         containers []]
    (let [is-hit? (= current hit)
          object (:object current)
          next-containers (if (some #{object} containers)
                            (filterv #(not= % object) containers)
                            (conj containers object))]
      (if is-hit?
        [(if (empty? containers)
           1.
           (get-in (last containers) [:material :refractive-index] 1.))
         (if (empty? next-containers)
           1.
           (get-in (last next-containers) [:material :refractive-index] 1.))]
        (if (empty? rest)
          [1. 1.]
          (recur rest next-containers))))))

; ## Prepare hit

; To help with the ray tracer computations, the `prepare-hit` function pre-computes some values and add them to an intersection:
; - the point in world space where the hit occured.
; - we also need to calculate the origin of the refracted ray, just under the surface at intersection.
; - the eye vector (pointing towards the camera).
; - the refractives indices `n` on both side of the hit
; - the normal of the object at the intersection point.
; - whether the intersection occurs on the inside of the object, in which case the normal is inverted.
; - the reflected ray direction

; To avoid acne syndrome, the computation should slightly displace the hit points (by epsilon) toward the outside/inside of the object.

(defn prepare-hit [hit ray ints]
  (let [point (r/pos ray (:t hit))
        normalv' (sh/normal (:object hit) point hit)
        eyev (t/neg (:direction ray))
        inside? (< (t/dot normalv' eyev) 0)
        normalv (if inside? (t/neg normalv') normalv')
        n (refractive-indices hit ints)]
    {:point (nc/axpy t/epsilon normalv point)
     :under-point (nc/axpy (- 0. t/epsilon) normalv point)
     :eyev eyev
     :n n
     :normalv normalv
     :inside? inside?
     :reflectv (t/reflect (:direction ray) normalv)}))

; ## Reflectance
;
; [[file:../samples/fresnel_example.png]]
;
; The `schlick` function returns a number between 0 and 1, inclusive.
; This number is called the reflectance and represents what fraction of the light is reflected, given the surface information at the hit.
; - reflectance is 1. when facing total internal reflection.
; - reflectance is close to 0. when incident ray is perpendicular to surface.
; - reflectance is significant when n 2 > n 1 and the ray strikes the surface at a small angle.

(defn schlick
  [hit]
  (let [{:keys [eyev normalv n]} hit
        [^double n1 ^double n2] n
        cos-i (t/dot eyev normalv)
        n-ratio (/ n1 n2)
        sin-t-square (* n-ratio n-ratio (- 1. (* cos-i cos-i)))]
    (if (and (> n1 n2)
             (> sin-t-square 1.))
      1.
      (let [cos-t (Math/sqrt (- 1. sin-t-square))
            cos (if (> n1 n2) cos-t cos-i)
            r0 (Math/pow (/ (- n1 n2) (+ n1 n2)) 2.)]
        (+ r0 (* (- 1. r0) (Math/pow (- 1. cos) 5)))))))
