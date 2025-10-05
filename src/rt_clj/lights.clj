; # Lights

(ns rt-clj.lights
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.colors :as c]
            [rt-clj.world-protocol :as wp]))

; ## Creation

; A point light has a position and intensity

(defn point-light [position intensity]
  {:position position
   :intensity intensity})

(defn shadowed
  [light world point]
  (if (wp/shadowed? world point (:position light))
    (assoc light :intensity c/black)
    light))
