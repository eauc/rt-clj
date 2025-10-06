(ns rt-clj.lights.sphere-light
  (:require [rt-clj.light-protocol :as lp]
            [rt-clj.tuples :as tu]
            [rt-clj.world-protocol :as wp]))

(defn shadow-factor
  [{:keys [radius]} world point light-position]
  (/
   (->> (map
         (fn [_]
           (let [pos (tu/add light-position (-> (tu/rand-vector 1.) (tu/norm) (tu/mul radius)))]
             (if (wp/shadowed? world point pos) 0. 1.)))
         (range lp/light-oversampling))
        (reduce +))
   lp/light-oversampling))

(defrecord SphereLight [radius]
  lp/Light
  (shadow-factor [shape world point light-position]
    (shadow-factor shape world point light-position)))

(defn sphere-light [radius]
  (->SphereLight radius))
