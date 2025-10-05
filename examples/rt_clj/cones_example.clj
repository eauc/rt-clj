; # Example: cones

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.cones-example
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
            [rt-clj.worlds :as wo])
  (:import java.lang.Math))

(let [filename "examples/img/cones-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [f-m {:color (co/color 0.2 0.2 0.2)
             :reflective 0.
             :transparency 1.
             :refractive-index 2.
             :shininess 300
             :shadow? false}
        floor (os/plane
               f-m
               (->> (tr/rotation-x (/ Math/PI 2))
                    (ma/mul (tr/translation 0. 0. -1.75))))
        w-m {:color (co/color 0.1 0.1 0.1)
             :reflective 1.
             :shininess 300}
        wall (os/plane
              w-m
              (tr/translation 0. -6. 0.))
        cone-1 (os/cone
                -3. tu/infinity false
                {:color (co/color 0.8 0.2 0.8)}
                (->> (tr/scaling 0.3 1. 0.3)
                     (ma/mul (tr/rotation-x (/ Math/PI 1.8)))))
        cone-2 (os/cone
                -2.5 -1.5 false
                {:color (co/color 0.2 0.8 0.8)}
                (->> (tr/scaling 2. 1. 2.)
                     (ma/mul (tr/rotation-x (/ Math/PI 1.8)))))
        cone-3 (os/cone
                -2. -1.5 false
                {:color (co/color 0.8 0.8 0.2)}
                (->> (tr/scaling 2.5 1. 2.5)
                     (ma/mul (tr/rotation-x (/ Math/PI 1.8)))))
        light-1 (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        world (wo/world {:objects [floor wall cone-1 cone-2 cone-3]
                         :lights [light-1]})
        view (tr/view (tu/point 4. 8. 2.5)
                      (tu/point 0. 0. -1.5)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
        ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    ;; print the PPM file
    (spit
     "./examples/img/cones-example.ppm"
     (prof/profile
      ; {:event :alloc}
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
