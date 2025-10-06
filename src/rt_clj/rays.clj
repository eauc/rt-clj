; # Rays

(ns rt-clj.rays
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.matrices :as m]
            [rt-clj.tuples :as t]))

; ## Creation
;
; Rays have a point as origin and a vector as direction.

(defrecord Ray [origin direction])

(defn ray [origin direction]
  (->Ray origin direction))

; ## Basic operations

; We can get the point at any distance from a ray's origin.

(defn pos [{:keys [origin  direction]} t]
  (t/add origin (t/mul direction t)))

; ## Transformations

; Translating a ray only translates the origin and doesn't change the direction.

; Scaling a ray scales both the origin and direction.

(defn transform [{:keys [origin direction]} t]
  {:origin (m/mul-t t origin)
   :direction (m/mul-t t direction)})
