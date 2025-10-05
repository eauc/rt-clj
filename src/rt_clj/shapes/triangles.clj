; # Triangles

(ns rt-clj.shapes.triangles
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Bounds

(def project
  (juxt t/x t/y t/z))

(defn- local-bounds
  [{:keys [p1 p2 p3]}]
  (let [[^double x1 ^double y1 ^double z1] (project p1)
        [^double x2 ^double y2 ^double z2] (project p2)
        [^double x3 ^double y3 ^double z3] (project p3)]
    {:min (t/point (min x1 x2 x3) (min y1 y2 y3) (min z1 z2 z3))
     :max (t/point (max x1 x2 x3) (max y1 y2 y3) (max z1 z2 z3))}))

; ## Intersection

; A ray that misses a triangle should not add any intersections to the intersection list.

; A ray that strikes a triangle should add exactly one intersection to the list.

; The specific algorithm that we’ll implement is the Möller–Trumbore algorithm:
; - cross the ray direction with e2,
; - then dot the result with e1 to produce the determinant.
; - if the result is close to zero, then the ray is parallel to the triangle and misses.

; An intersection record may have u and v properties, to help identify where on a triangle the intersection occurred, relative to the triangle’s corners.

(defn- local-intersect
  [{:keys [p1 e1 e2]}
   {:keys [origin direction]}
   object]
  (let [dir><e2 (t/cross direction e2)
        d (t/dot e1 dir><e2)]
    (if (t/close? 0. d)
      []
      (let [f (/ 1. d)
            p1->origin (t/sub origin p1)
            u (* f (t/dot p1->origin dir><e2))]
        (if-not (<= 0. u 1.)
          []
          (let [origin><e1 (t/cross p1->origin e1)
                v (* f (t/dot direction origin><e1))]
            (if (or (> 0 v)
                    (< 1 (+ u v)))
              []
              [(assoc (i/intersection
                       (* f (t/dot e2 origin><e1))
                       object)
                      :u u :v v)])))))))

; ## Normal

; The triangle’s precomputed normal is used for every point on the triangle.

(defn- local-normal
  [{:keys [normal]}]
  normal)

; ## Creation

; We pre-compute 2 edges vectors and the normal vector at creation.

(defrecord Triangle [p1 p2 p3 e1 e2 normal]
  sh/Shape
  (local-bounds [tri]
    (local-bounds [tri]))
  (local-intersect [tri ray object]
    (local-intersect tri ray object))
  (local-normal [tri _ _]
    (local-normal tri)))

(defn triangle
  ([p1 p2 p3]
   (let [e1 (t/sub p2 p1)
         e2 (t/sub p3 p1)]
     (->Triangle p1 p2 p3 e1 e2 (t/norm (t/cross e2 e1))))))

; ## Smooth Triangles

; A smooth triangle should store the triangle’s three vertex points, as well as the normal vector at each of those points.

; When computing the normal vector on a smooth triangle, use the intersection’s u and v properties to interpolate the normal.

(defn- smooth-local-normal
  [{:keys [n1 n2 n3]} {:keys [^double u ^double v]}]
  (-> (t/add (t/add (t/mul n1 (- 1. u v))
                    (t/mul n2 u))
             (t/mul n3 v))
      t/norm))

(defrecord SmoothTriangle [p1 p2 p3 e1 e2 n1 n2 n3]
  sh/Shape
  (local-bounds [tri]
    (local-bounds tri))
  (local-intersect [tri ray object]
    (local-intersect tri ray object))
  (local-normal [tri _ hit]
    (smooth-local-normal tri hit)))

(defn smooth-triangle
  ([p1 p2 p3 n1 n2 n3]
   (let [e1 (t/sub p2 p1)
         e2 (t/sub p3 p1)]
     (->SmoothTriangle p1 p2 p3 e1 e2 n1 n2 n3))))
