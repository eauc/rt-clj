; # Lights

(ns rt-clj.lights
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.colors :as co]
            [rt-clj.light-protocol :as lp]
            [rt-clj.lights.point-light :as pl]
            [rt-clj.lights.sphere-light :as sl]
            [rt-clj.lights.spot-light :as sp]))

; ## Creation

; A point light has a position and intensity

(defrecord Light [shape position intensity])

(defn light [shape position intensity]
  (->Light shape position intensity))

(defn point-light [position intensity]
  (light (pl/point-light) position intensity))

(defn sphere-light [radius position intensity]
  (light (sl/sphere-light radius) position intensity))

(defn spot-light [direction width fade-factor position intensity]
  (light (sp/spot-light direction width fade-factor) position intensity))

(defn shadowed
  [light world point]
  (let [{:keys [shape intensity position]} light
        f (lp/shadow-factor shape world point position)]
    (assoc light :intensity (co/mul intensity f))))
