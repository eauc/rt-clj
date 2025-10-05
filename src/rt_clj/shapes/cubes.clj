; # Cubes

(ns rt-clj.shapes.cubes
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Intersections

; This works by treating a cube as it were composed of six planes, one for each face of the cube.

; Intersecting a ray with that cube involves testing it against each of the planes,
; and if the ray intersects them in just the right way, it means that the ray intersects the cube, as well.
; - The first step is to find the t values of all the places where the ray intersects those planes.
; - For each pair of planes, there will be a minimum t closest to the ray origin, and a maximum t farther away.
; - Focus on the largest of all the minimum t values and the smallest of all the maximum t values.
; - The intersection of the ray with that square will always be those two points: the largest minimum t value and the smallest maximum t value.
; - If the largest minimum t value is greater than the smallest maximum t value, the ray misses the cube.

(defn check-axis
  [^double origin ^double direction ^double min ^double max]
  (let [t-min-numerator (- min origin)
        t-max-numerator (- max origin)
        parallel? (< (Math/abs direction) (double t/epsilon))
        t-min (if parallel?
                (* t-min-numerator (double t/infinity))
                (/ t-min-numerator direction))
        t-max (if parallel?
                (* t-max-numerator (double t/infinity))
                (/ t-max-numerator direction))]
    (if (> t-min t-max)
      [t-max t-min]
      [t-min t-max])))

(defn- local-intersect
  [{:keys [origin direction]} ;; ray
   object]
  (let [[^double x-t-min ^double x-t-max] (check-axis (t/x origin) (t/x direction) -1 1)
        [^double y-t-min ^double y-t-max] (check-axis (t/y origin) (t/y direction) -1 1)
        [^double z-t-min ^double z-t-max] (check-axis (t/z origin) (t/z direction) -1 1)
        t-min (clojure.core/max x-t-min y-t-min z-t-min)
        t-max (clojure.core/min x-t-max y-t-max z-t-max)]
    (if (> t-min t-max)
      []
      [(i/intersection t-min object)
       (i/intersection t-max object)])))

; ## Normal

;  Each face of a cube is a plane with its own normal. This normal will be the same at every point on the corresponding face.

(defn- local-normal
  [point]
  (let [x-abs (Math/abs (t/x point))
        y-abs (Math/abs (t/y point))
        z-abs (Math/abs (t/z point))
        maxc (max x-abs y-abs z-abs)]
    (condp = maxc
      x-abs (t/vector (t/x point) 0. 0.)
      y-abs (t/vector 0. (t/y point) 0.)
      (t/vector 0. 0. (t/z point)))))

; ## Creation

; An axis-aligned bounding box, or AABB, is a box with a special property: its sides are all aligned with the scene’s axes. 

; Two are aligned with the x axis, two with the y axis, and two with the z axis.

(defrecord Cube []
  sh/Shape
  (local-bounds [_]
    {:min (t/point -1. -1. -1.)
     :max (t/point 1. 1. 1.)})
  (prepare-transform [shape _ _]
    shape)
  (includes? [_ _]
    false)
  (local-intersect [_ ray object]
    (local-intersect ray object))
  (local-normal [_ point _]
    (local-normal point)))

(defn cube []
  (->Cube))
