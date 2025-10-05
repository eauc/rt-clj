; # Example: planes and spheres
;
; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.planes-spheres-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
;            [criterium.core :as criterium]
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

(let [filename "examples/img/planes-spheres-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [w-material {:color (co/color 1. 0.9 0.9)
                    :specular 0.}
        floor (-> (os/plane)
                  (os/with-material w-material))
        wall (os/plane
              w-material
              (->> (tr/rotation-x (/ Math/PI 2))
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
        light (li/point-light (tu/point -10. 10. -10.) (co/color 1. 1. 1.))
        world (wo/world [floor wall
                         middle right]
                        [light])
        view (tr/view (tu/point 0. 1.5 -5.)
                      (tu/point 0. 1. 0.)
                      (tu/vector 0. 1. 0.))
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
    (prof/profile
     (spit
      "./examples/img/planes-spheres-example.ppm"
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
