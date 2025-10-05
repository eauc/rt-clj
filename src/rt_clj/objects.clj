; # Objects

(ns rt-clj.objects
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.shapes.cones :as co]
            [rt-clj.shapes.csg-shapes :as csg]
            [rt-clj.shapes.cylinders :as cy]
            [rt-clj.shapes.cubes :as cu]
            [rt-clj.shapes.groups :as gr]
            [rt-clj.materials :as mr]
            [rt-clj.matrices :as m]
            [rt-clj.object-protocol :as o]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.shapes.planes :as p]
            [rt-clj.shapes.spheres :as s]
            [rt-clj.shapes.triangles :as tr]
            [rt-clj.tuples :as t]))

; ## Intersections

; To calculate the world-intersect, we must first transform the ray in the object coordinates.

(defn intersect [{:keys [shape inverse-t] :as object} ra]
  (let [local-ray (r/transform ra inverse-t)]
    (sh/local-intersect shape local-ray object)))

; ## World transformations

; `world->object` takes a point in world space and transform it to object space, taking into consideration any parent objects between the two spaces.

(defn world->object
  [{:keys [parent inverse-t] :as object} point]
  (if (nil? object)
    point
    (m/mul-t inverse-t (world->object parent point))))

; `object->world` takes a normal vector in object space and transform it to world space, taking into consideration any parent objects between the two spaces.

(defn object->world
  [{:keys [trans-inverse-t parent]} v]
  (let [[x y z] ((juxt t/x t/y t/z) (m/mul-t trans-inverse-t v))
        new-v (t/norm (t/vector x y z))]
    (if (nil? parent)
      new-v
      (object->world parent new-v))))

; ## Normal

; `shape/normal` must find the normal on a child object of a group, taking into account transformations on both the child object and the parent(s).

; To calculate the world-normal, we must :
; - first transform the intersection point into object-world.
; - then calculate the local-normal in object-world easily.
; - we must then transform this normal back, using the transpose inverse of the transformation matrix of the object.
; - this calculation results in a wrong =w= component, so we just trop it.
; - the resulting vector is also not normalized anymore, so we normalize the result.

(defn normal [{:keys [shape] :as object} world-point hit]
  (let [local-point (world->object object world-point)
        local-normal (sh/local-normal shape local-point hit)]
    (object->world object local-normal)))

; ## Children

; Children contains a reference to their parent group.

(defn with-parent
  [object {:keys [material] :as parent}]
  (let [object' (cond-> object
                  :always (assoc :parent parent)
                  (some? material) (assoc :material material))]
    (assoc object' :children (mapv #(with-parent % object') (:children object')))))

; ## Creation

(defrecord WorldObject [shape material transform inverse-t trans-inverse-t parent]
  o/WorldObject
  (intersect [object ray]
    (intersect object ray))
  (normal [object point hit]
    (normal object point hit)))

(defn object [shape material transform]
  (let [inverse-t (m/inverse transform)]
    (->WorldObject
     shape
     (merge mr/default-material material)
     transform
     inverse-t
     (m/transpose inverse-t)
     nil)))

(defn cone
  ([minimum maximum closed? material transform]
   (object (co/cone minimum maximum closed?) material transform))
  ([minimum maximum closed?]
   (cone minimum maximum closed? mr/default-material (m/id 4))))

(defn cylinder
  ([minimum maximum closed? material transform]
   (object (cy/cylinder minimum maximum closed?) material transform))
  ([minimum maximum closed?]
   (cylinder minimum maximum closed? mr/default-material (m/id 4))))

(defn csg-shape
  ([operation left right material transform]
   (let [c (object (csg/csg-shape operation left right) material transform)]
     (-> c
         (update-in [:shape left] with-parent c)
         (update-in [:shape right] with-parent c))))
  ([operation left right]
   (csg-shape operation left right mr/default-material (m/id 4))))

(defn cube
  ([material transform]
   (object (cu/cube) material transform))
  ([]
   (cube mr/default-material (m/id 4))))

(defn group
  ([children material transform]
   (let [gr (object (gr/group children) material transform)]
     (update-in gr [:shape :children] #(mapv (fn [c] (with-parent c gr)) %))))
  ([children]
   (group children mr/default-material (m/id 4))))

(defn plane
  ([material transform]
   (object (p/plane) material transform))
  ([]
   (plane mr/default-material (m/id 4))))

(defn sphere
  ([material transform]
   (object (s/sphere) material transform))
  ([]
   (sphere mr/default-material (m/id 4))))

(defn triangle
  ([p1 p2 p3 material transform]
   (object (tr/triangle p1 p2 p3) material transform))
  ([p1 p2 p3]
   (triangle p1 p2 p3 mr/default-material (m/id 4))))

(defn smooth-triangle
  ([p1 p2 p3 n1 n2 n3 material transform]
   (object (tr/smooth-triangle p1 p2 p3 n1 n2 n3) material transform))
  ([p1 p2 p3 n1 n2 n3]
   (smooth-triangle p1 p2 p3 n1 n2 n3 mr/default-material (m/id 4))))

(defn with-material [object material-options]
  (assoc object :material (merge mr/default-material
                                 material-options)))

(defn with-transform [object transform]
  (let [inverse-t (m/inverse transform)]
    (assoc object
           :transform transform
           :inverse-t inverse-t
           :trans-inverse-t (m/transpose inverse-t))))
