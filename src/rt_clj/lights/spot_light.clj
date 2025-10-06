(ns rt-clj.lights.spot-light
  (:require [clojure.math :as m]
            [rt-clj.light-protocol :as lp]
            [rt-clj.tuples :as tu]
            [rt-clj.world-protocol :as wp]))

(defn- shadowed
  [{:keys [direction full-width width fade-width]} world point light-position]
  (if (wp/shadowed? world point light-position)
    0.
    (let [light->point (-> (tu/sub point light-position) tu/norm)
          cos (tu/dot direction light->point)
          azimuth (m/acos cos)]
      (if (> 0 cos)
        0.
        (if (> azimuth full-width)
          0.
          (if (> azimuth width)
            (let [dt (- azimuth width)]
              (- 1. (Math/pow (/ dt fade-width) 2)))
            1.))))))

(defrecord SpotLight [direction full-width width fade-width]
  lp/Light
  (lp/shadow-factor [shape world point light-position]
    (shadowed shape world point light-position)))

(defn spot-light [direction full-width fade-factor]
  (->SpotLight
   (tu/norm direction)
   full-width
   (* fade-factor full-width)
   (- full-width (* fade-factor full-width))))
