; # Example: simple world with 3 lights and 3 spheres

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.multi-lights-example
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
        lights [(li/point-light (tu/point 3. 5. 0.) (co/color 1. 0. 0.))
                (li/point-light (tu/point 3. 5. -5.) (co/color 0. 1. 0.))
                (li/point-light (tu/point 3. 5. 5.) (co/color 0. 0. 1.))]
        world (wo/world {:objects [floor sphere]
                         :lights lights})
        view (tr/view (tu/point 5. 3. 0.)
                      (tu/point 0. 1. 0.)
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
     "./examples/img/multi-lights-example.ppm"
     (prof/profile
      ; {:event :alloc}
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
