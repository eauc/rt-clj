; # Example: rays intersections with a sphere

(ns rt-clj.sphere-ray-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
            [nextjournal.clerk :as clerk]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.intersections :as in]
            [rt-clj.rays :as ra]
            [rt-clj.object-protocol :as o]
            [rt-clj.objects :as os]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]))

; Let's draw a sphere, in `[0. 0. 0.]`, scaled by a factor `0.5` in the `y` direction.
;
; The rays origin is in `[3. 0. 0.]` and the projection screen is the plane `[-3. y z]`.
;
; Pixels will be black if the ray misses, red if the ray hits the sphere.

(let [filename "examples/img/sphere-ray-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [miss-col (co/color 0. 0. 0.)
        hit-col (co/color 1. 0. 0.)
        s (-> (os/sphere) 
              (os/with-transform (tr/scaling 1. 0.5 1.)))
        o (tu/point 3. 0. 0.)
        h 256
        w 256
        sc-min -3.
        sc-max 3.
        sc-width (- sc-max sc-min)
        pixel-step-w (/ sc-width w)
        pixel-step-h (/ sc-width h)
        hits (mapv (fn [i]
                     (mapv (fn [j]
                             (let [screen-p (tu/point -3.
                                                      (+ -3. (* i pixel-step-w))
                                                      (+ -3. (* j pixel-step-h)))
                                   ray-d (tu/sub screen-p o)
                                   ray (ra/ray o ray-d)]
                               (in/hit (o/intersect s ray)))) (range w))) (range h))
        cv (reduce (fn [c i]
                     (reduce (fn [c j]
                               (ca/assoc-at c i j (if (get-in hits [i j])
                                                    hit-col
                                                    miss-col)))
                             c (range w)))
                   (ca/canvas w h) (range h))]
    ;; print the PPM file
    (spit "./examples/img/sphere-ray-example.ppm"
          (clojure.string/join "\n" (ca/ppm-rows cv)))))
