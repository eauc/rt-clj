(ns rt-clj.object-protocol)

(defprotocol WorldObject
  (prepare-bounds [object] "Pre-computes the boundaries of the object to optimize intersections")
  (prepare-transform [object world->object object->world] "Precomputes transform matrices recursively")
  (prepare [object] "Precomputes object properties before rendering")
  (includes? [object needle] "Checks if object (or its children) includes the needle")
  (intersect [object ray] "Intersects the object with ray in world space")
  (normal [object point hit] "Returns the normal on the object at point in world space"))
