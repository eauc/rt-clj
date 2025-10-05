(ns rt-clj.object-protocol)

(defprotocol WorldObject
  (intersect [object ray] "Intersects the object with ray in world space")
  (normal [object point hit] "Returns the normal on the object at point in world space"))
