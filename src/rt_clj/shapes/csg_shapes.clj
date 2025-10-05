; # CSG Shapes

(ns rt-clj.shapes.csg-shapes
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.bounds :as bd]
            [rt-clj.object-protocol :as o]
            [rt-clj.shape-protocol :as sh]))

; [[file:../samples/csg_spheres_example.png]]
;
; [[file:../samples/csg_cube_example.png]]
;
; [[file:../samples/csg_lens_example.png]]

; ## Intersections

; A CSG union preserves all intersections on the exterior of both shapes.
;
; A CSG intersect preserves all intersections where both shapes overlap.
;
; A CSG difference preserves all intersections not exclusively inside the object on the right.

; Given a set of intersections, produce a subset of only those intersections that conform to the operation of the current CSG object.

; A ray should intersect a CSG object if it intersects any of its children.

(defmulti intersection-allowed (fn [op _ _ _] op))

(defmethod intersection-allowed :default
  [_ _ _ _]
  false)

(defmethod intersection-allowed :union
  [_ lhit inl inr]
  (or (and lhit (not inr))
      (and (not lhit) (not inl))))

(defmethod intersection-allowed :intersection
  [_ lhit inl inr]
  (or (and lhit inr)
      (and (not lhit) inl)))

(defmethod intersection-allowed :difference
  [_ lhit inl inr]
  (or (and lhit (not inr))
      (and (not lhit) inl)))

(defn filter-intersections
  [{:keys [operation left]} ints]
  (loop [[{:keys [object] :as int} & rest] ints
         inl false
         inr false
         result []]
    (if (nil? int)
      result
      (let [lhit (o/includes? left object)
            allowed? (intersection-allowed operation lhit inl inr)
            result' (if allowed? (conj result int) result)
            inl' (if lhit (not inl) inl)
            inr' (if lhit inr (not inr))]
        (recur rest inl' inr' result')))))

(defn- local-intersect
  [{:keys [left right] :as shape} ray {:keys [bounds]}]
  (if-not (bd/intersect? bounds ray)
    []
    (->> (concat (o/intersect left ray)
                 (o/intersect right ray))
         (sort-by :t)
         (filter-intersections shape))))

; ## Creation

; A CSG shape is composed of an operation and two operand shapes.

(defrecord CSGShape [operation left right]
  sh/Shape
  (local-bounds [{:keys [left right]}]
    (let [left-bs (bd/transform (:bounds left) (:transform left))
          right-bs (bd/transform (:bounds right) (:transform right))]
      (bd/merge [left-bs right-bs])))
  (prepare-bounds [{:keys [left right] :as shape}]
    (assoc shape
           :left (o/prepare-bounds left)
           :right (o/prepare-bounds right)))
  (prepare-transform [{:keys [left right] :as shape} world->object object->world]
    (assoc shape
           :left (o/prepare-transform left world->object object->world)
           :right (o/prepare-transform right world->object object->world)))
  (includes? [{:keys [left right]} needle]
    (or (o/includes? left needle)
        (o/includes? right needle)))
  (local-intersect [csg ray object]
    (local-intersect csg ray object))
  (local-normal [_ _ _]
    (throw (ex-info "We should never call local-normal on a CSG Shape." {}))))

(defn csg-shape
  ([operation left right]
   (->CSGShape operation left right)))
