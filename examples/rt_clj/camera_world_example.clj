; # Example: simple world with 3 worlds and 3 spheres

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.camera-world-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
            ; [criterium.core :as criterium]
            [clj-async-profiler.core :as prof]
            [nextjournal.clerk :as clerk]
            [rt-clj.cameras :as cm]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.lights :as li]
            [rt-clj.matrices :as ma]
            [rt-clj.objects :as os]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]
            [rt-clj.worlds :as wo]))

(let [filename "examples/img/camera-world-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [w-material {:color (co/color 1. 0.9 0.9)
                    :specular 0.}
        floor (os/sphere
               w-material
               (tr/scaling 10. 0.01 10.))
        left-wall (os/sphere
                   w-material
                   (->> (tr/scaling 10. 0.01 10.)
                        (ma/mul (tr/rotation-x (/ Math/PI 2)))
                        (ma/mul (tr/rotation-y (- (/ Math/PI 4))))
                        (ma/mul (tr/translation 0. 0. 5.))))
        right-wall (os/sphere
                    w-material
                    (->> (tr/scaling 10. 0.01 10.)
                         (ma/mul (tr/rotation-x (/ Math/PI 2)))
                         (ma/mul (tr/rotation-y (/ Math/PI 4)))
                         (ma/mul (tr/translation 0. 0. 5.))))
        middle (os/sphere
                {:color (co/color 0.1 1. 0.5)
                 :diffuse 0.7
                 :specular 0.3}
                (tr/translation -0.5 1. 0.5))
        right (os/sphere
               {:color (co/color 0.5 1. 0.1)
                :diffuse 0.7
                :specular 0.3}
               (->> (tr/scaling 0.5 0.5 0.5)
                    (ma/mul (tr/translation 1.5 0.5 -0.5))))
        left (os/sphere
              {:color (co/color 1. 0.8 0.1)
               :diffuse 0.7
               :specular 0.3}
              (->> (tr/scaling 0.33 0.33 0.33)
                   (ma/mul (tr/translation -1.5 0.33 -0.75))))
        light (li/point-light (tu/point -10. 10. -10.) (co/color 1. 1. 1.))
        world (wo/world [floor left-wall right-wall
                         middle left right]
                        [light])
        view (tr/view (tu/point 0. 1.5 -5.)
                      (tu/point 0. 1. 0.)
                      (tu/vector 0. 1. 0.))
        resolution 4
        cam (cm/camera (* resolution 150) (* resolution 100) (/ Math/PI 3) view)]
        ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    ;; print the PPM file
    (spit
     "./examples/img/camera-world-example.ppm"
     (prof/profile
      ; {:event :alloc}
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
