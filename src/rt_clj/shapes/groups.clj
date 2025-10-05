; # Groups

(ns rt-clj.shapes.groups
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.object-protocol :as o]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; (defn children-bounds
;   [cs]
;   (let [bs (map sh/bounds cs)
;         min-xs (map (comp t/x :min) bs)
;         min-ys (map (comp t/y :min) bs)
;         min-zs (map (comp t/z :min) bs)
;         max-xs (map (comp t/x :max) bs)
;         max-ys (map (comp t/y :max) bs)
;         max-zs (map (comp t/z :max) bs)]
;     {:min (t/point (apply min min-xs)
;                    (apply min min-ys)
;                    (apply min min-zs))
;      :max (t/point (apply max max-xs)
;                    (apply max max-ys)
;                    (apply max max-zs))}))

; ## Intersections

; Intersecting a ray with a empty group should always return no intersections.

; Otherwise, it should returns the conjunction of all intersections with each child shape, sorted by increasing distance.

; It should correctly apply the group and its children transformations.

(defn local-intersect
  [{:keys [children]} ray]
  (->> (reduce (fn [ints c] (concat ints (o/intersect c ray))) '() children)
       (sort-by :t)
       (into [])))

; ## Creation

(defrecord Group [children]
  sh/Shape
  (local-bounds [_]
    (let [e (double t/epsilon)]
      {:min (t/point (- e) (- e) (- e))
       :max (t/point e e e)}))
  (local-intersect [gr ray _]
    (local-intersect gr ray))
  (local-normal [_ _ _]
    (throw (ex-info "We should never call local-normal on a group." {}))))

(defn group
  [children]
  (->Group children))
