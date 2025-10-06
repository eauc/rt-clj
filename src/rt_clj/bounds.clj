; # Bounds

(ns rt-clj.bounds
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:refer-clojure :exclude [merge min max])
  (:require [rt-clj.matrices :as m]
            [rt-clj.tuples :as t]))

(defn bounds
  [min max]
  {:min min
   :max max})

(def infinite
  (bounds
   (t/point (- ^double t/infinity) (- ^double t/infinity) (- ^double t/infinity))
   (t/point t/infinity t/infinity t/infinity)))

(def default
  (bounds
   (t/point -1. -1. -1.)
   (t/point 1. 1. 1.)))

(defn merge
  [bs]
  (let [xs (mapcat (juxt (comp t/x :max) (comp t/x :min)) bs)
        ys (mapcat (juxt (comp t/y :max) (comp t/y :min)) bs)
        zs (mapcat (juxt (comp t/z :max) (comp t/z :min)) bs)]
    {:min (t/point (apply clojure.core/min xs)
                   (apply clojure.core/min ys)
                   (apply clojure.core/min zs))
     :max (t/point (apply clojure.core/max xs)
                   (apply clojure.core/max ys)
                   (apply clojure.core/max zs))}))

; To calculate the world-boudaries of a shape:
; - calculate all 8 corners of the boundaries in local space.
; - transform each corner into world space.
; - take the min and max of all =x, y, z= coordinates.

(def project
  (juxt t/x t/y t/z))

(defn transform
  [bounds t]
  (let [{:keys [min max]} bounds
        [x-min y-min z-min] (project min)
        [x-max y-max z-max] (project max)
        corners (map #(m/mul-t t %)
                     [(t/point x-min y-min z-min)
                      (t/point x-min y-min z-max)
                      (t/point x-min y-max z-min)
                      (t/point x-min y-max z-max)
                      (t/point x-max y-min z-min)
                      (t/point x-max y-min z-max)
                      (t/point x-max y-max z-min)
                      (t/point x-max y-max z-max)])
        xs (map t/x corners)
        ys (map t/y corners)
        zs (map t/z corners)]
    {:min (t/point (apply clojure.core/min xs)
                   (apply clojure.core/min ys)
                   (apply clojure.core/min zs))
     :max (t/point (apply clojure.core/max xs)
                   (apply clojure.core/max ys)
                   (apply clojure.core/max zs))}))

(defn check-axis
  [^double origin ^double direction ^double min ^double max]
  (let [t-min-numerator (- min origin)
        t-max-numerator (- max origin)
        parallel? (< (Math/abs direction) (double t/epsilon))
        t-min (if parallel?
                (* t-min-numerator (double t/infinity))
                (/ t-min-numerator direction))
        t-max (if parallel?
                (* t-max-numerator (double t/infinity))
                (/ t-max-numerator direction))]
    (if (> t-min t-max)
      [t-max t-min]
      [t-min t-max])))

(defn intersect?
  [bounds ray]
  (let [{:keys [min max]} bounds
        {:keys [origin direction]} ray
        [^double x-t-min ^double x-t-max] (check-axis (t/x origin) (t/x direction) (t/x min) (t/x max))
        [^double y-t-min ^double y-t-max] (check-axis (t/y origin) (t/y direction) (t/y min) (t/y max))
        [^double z-t-min ^double z-t-max] (check-axis (t/z origin) (t/z direction) (t/z min) (t/z max))
        t-min (clojure.core/max x-t-min y-t-min z-t-min)
        t-max (clojure.core/min x-t-max y-t-max z-t-max)]
    (<= t-min t-max)))
  
