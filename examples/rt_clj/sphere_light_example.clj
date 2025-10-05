; # Example: one sphere and one light

(ns rt-clj.sphere-light-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
            [nextjournal.clerk :as clerk]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.intersections :as in]
            [rt-clj.lights :as li]
            [rt-clj.materials :as mr]
            [rt-clj.object-protocol :as o]
            [rt-clj.objects :as os]
            [rt-clj.rays :as ra]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]))

; Let's draw a sphere, in `[0. 0. 0.]`, scaled by a factor `0.5` in the `z` direction.
;
; The rays origin is in `[3. 0. 0.]` and the projection screen is the plane `[-3. y z]`.
;
; A light source is behind the eye to the upper right at `[10. 10. -10.]`
;
; The sphere is purple and we will use our material lighting.

(let [filename "examples/img/sphere-light-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [miss-col (co/color 0. 0. 0.)
        l (li/point-light (tu/point 10. 10. -10.) (co/color 1. 1. 1.))
        m (-> mr/default-material
              (assoc :color (co/color 1. 0.2 1.)))
        s (-> (-> (os/sphere) 
                  (os/with-transform (tr/scaling 1. 1. 0.5)))
              (assoc :material m))
        e (tu/point 3. 0. 0.)
        h 512
        w 512
        sc-min -3.
        sc-max 3.
        sc-width (- sc-max sc-min)
        pixel-step-w (/ sc-width w)
        pixel-step-h (/ sc-width h)
        pixel (fn [i j]
                (let [screen-p (tu/point -3.
                                         (+ -3. (* i pixel-step-w))
                                         (+ -3. (* j pixel-step-h)))
                      ray-d (tu/norm (tu/sub screen-p e))
                      ray (ra/ray e ray-d)
                      hit? (in/hit (o/intersect s ray))]
                  (if (nil? hit?)
                    miss-col
                    (let [position (ra/pos ray (:t hit?))
                          normal (o/normal s position hit?)]
                      (mr/lighting m s l position ray-d normal false)))))
        pixs (mapv (fn [i]
                     (mapv (fn [j]
                             (pixel i j))
                           (range w)))
                   (range h))
        cv (reduce (fn [c i]
                     (reduce (fn [c j]
                               (ca/assoc-at c i j (get-in pixs [i j])))
                             c (range w)))
                   (ca/canvas w h) (range h))]
    ;; print the PPM file
    (spit "./examples/img/sphere-light-example.ppm"
          (clojure.string/join "\n" (ca/ppm-rows cv)))))
