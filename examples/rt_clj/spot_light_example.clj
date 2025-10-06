; # Example: simple world with 3 lights and 3 spheres

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.spot-light-example
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
            [rt-clj.objects :as os]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]
            [rt-clj.worlds :as wo]))

(let [filename "examples/img/multi-lights-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [floor (os/plane
               nil
               (tr/translation 0. -1. 0.))
        sphere (os/sphere
                nil
                (tr/translation 0. 1. 0.))
        light-origin (tu/point 3. 5. -2.)
        lights [(li/spot-light
                 (tu/sub tu/origin light-origin)
                 (/ Math/PI 8)
                 0.3
                 light-origin co/white)]
        world (wo/world {:objects [floor sphere]
                         :lights lights})
        view (tr/view (tu/point 6. 3. 0.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 1. 0.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view
                        :parallel-depth 8})]
        ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    ;; print the PPM file
    (spit
     "./examples/img/spot-light-example.ppm"
     (prof/profile
      ; {:event :alloc}
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
