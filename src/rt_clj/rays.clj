; # Rays

(ns rt-clj.rays
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.matrices :as m]))

; ## Creation
;
; Rays have a point as origin and a vector as direction.

(defrecord Ray [origin direction])

(defn ray [origin direction]
  (->Ray origin direction))

; ## Basic operations

; We can get the point at any distance from a ray's origin.

(defn pos ^"[D" [{:keys [^"[D" origin ^"[D" direction]} ^double t]
  (let [r (aclone origin)]
    (dotimes [k (alength r)]
      (aset r k (+ (aget origin k)
                   (* t (aget direction k)))))
    r))

; ## Transformations

; Translating a ray only translates the origin and doesn't change the direction.

; Scaling a ray scales both the origin and direction.

(defn transform [{:keys [origin direction]} t]
  {:origin (m/mul-t t origin)
   :direction (m/mul-t t direction)})
