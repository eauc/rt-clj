(ns rt-clj.lights.point-light
  (:require [rt-clj.light-protocol :as lp]
            [rt-clj.world-protocol :as wp]))

(defn shadow-factor
  [world point light-position]
  (if (wp/shadowed? world point light-position)
    0.
    1.))

(defrecord PointLight []
  lp/Light
  (shadow-factor [_ world point light-position]
    (shadow-factor world point light-position)))

(defn point-light []
  (->PointLight))
