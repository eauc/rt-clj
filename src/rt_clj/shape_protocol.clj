; # Shape

(ns rt-clj.shape-protocol
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true})

; ## Generic Shapes

(defprotocol Shape
  (local-bounds [shape] "Returns the bounds of the shape in local space")
  (local-intersect [shape ray object] "intersects the shape with a ray in local space")
  (local-normal [shape point hit] "returns the normal on the shape at point in local space"))
