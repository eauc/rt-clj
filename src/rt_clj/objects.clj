; # Objects

(ns rt-clj.objects
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.bounds :as bd]
            [rt-clj.shapes.cones :as co]
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
            [rt-clj.tuples :as tu]))

(defn- includes?
  [object needle]
  (let [{:keys [shape]} object]
    (if (= object needle)
      true
      (sh/includes? shape object))))

; ## Intersections

(defn- prepare-bounds
  [{:keys [shape] :as object}]
  (let [shape' (sh/prepare-bounds shape)]
    (assoc object 
           :shape shape'
           :bounds (sh/local-bounds shape'))))

; To calculate the world-intersect, we must first transform the ray in the object coordinates.

(defn intersect [{:keys [shape inverse-t] :as object} ra]
  (let [local-ray (r/transform ra inverse-t)]
    (sh/local-intersect shape local-ray object)))

; ## World transformations

(defn- prepare-transform
  ([object parent-world->object parent-object->world]
   (let [{:keys [inverse-t shape]} object
         world->object (m/mul inverse-t parent-world->object)
         object->world (m/mul parent-object->world (m/transpose inverse-t))]
     (assoc object
            :world->object world->object
            :object->world object->world
            :shape (sh/prepare-transform shape world->object object->world))))
  ([object]
   (prepare-transform object (m/id 4) (m/id 4))))

; ## Normal

; `shape/normal` must find the normal on a child object of a group

; To calculate the world-normal, we must :
; - first transform the intersection point into object-world.
; - then calculate the local-normal in object-world easily.
; - we must then transform this normal back, using the transpose inverse of the transformation matrix of the object.
; - this calculation results in a wrong =w= component, so we just trop it.
; - the resulting vector is also not normalized anymore, so we normalize the result.

(defn normal [{:keys [shape] :as object} world-point hit]
  (let [local-point (m/mul-t (:world->object object) world-point)
        local-normal (sh/local-normal shape local-point hit)
        world-normal (m/mul-t (:object->world object) local-normal)]
    (-> world-normal
        tu/to-vector!
        tu/norm)))

; ## Creation

(defrecord WorldObject [shape material transform inverse-t world->object object->world bounds]
  o/WorldObject
  (prepare-bounds [object]
    (prepare-bounds object))
  (prepare-transform [object world->object object->world]
    (prepare-transform object world->object object->world))
  (prepare [object]
    (prepare-bounds object)
    (prepare-transform object))
  (includes? [object needle]
    (includes? object needle))
  (intersect [object ray]
    (intersect object ray))
  (normal [object point hit]
    (normal object point hit)))

(defn with-material [object material-options]
  (assoc object :material (merge mr/default-material
                                 material-options)))

(defn with-transform [object transform]
  (let [inverse-t (m/inverse transform)]
    (assoc object
           :transform transform
           :inverse-t inverse-t
           :world->object inverse-t
           :object->world (m/transpose inverse-t))))

(defn object [shape material transform]
  (-> (map->WorldObject {:shape shape :bounds bd/infinite})
      (with-material material)
      (with-transform transform)))

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
   (object (csg/csg-shape operation left right) material transform))
  ([operation left right]
   (csg-shape operation left right mr/default-material (m/id 4))))

(defn cube
  ([material transform]
   (object (cu/cube) material transform))
  ([]
   (cube mr/default-material (m/id 4))))

(defn group
  ([children material transform]
   (object (gr/group children) material transform))
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
