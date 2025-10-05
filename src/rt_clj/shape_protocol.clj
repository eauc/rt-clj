; # Shape

(ns rt-clj.shape-protocol
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true})

; ## Generic Shapes

(defprotocol Shape
  (local-bounds [shape] "Returns the bounds of the shape in local space")
  (prepare-bounds [shape] "Recursively precompute the boundaries of child objects")
  (prepare-transform [shape world->object object->world] "Recursively precompute transformations matrices")
  (includes? [shape needle] "Checks if shape (or its children) includes the needle")
  (local-intersect [shape ray object] "intersects the shape with a ray in local space")
  (local-normal [shape point hit] "returns the normal on the shape at point in local space"))
