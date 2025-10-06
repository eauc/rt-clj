; # Example: simple world with 3 lights and 3 spheres

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.blur-example
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
  (let [floor (-> (os/plane)
                  (os/with-material
                    {:ambient 0
                     :diffuse 0.8
                     :specular 0.5}))
        red (os/sphere
             {:color (co/color 1. 0. 0.)}
             (tr/translation 0. 1.5 0.))
        green (os/sphere
                {:color (co/color 0. 1. 0.)}
                (-> (tr/translation 2. 1. 1.)
                    (ma/mul (tr/scaling 0.5 0.5 0.5))))
        blue (os/sphere
               {:color (co/color 0. 0. 1.)}
               (tr/translation -3. 1.5 -4.))
        lights [(li/point-light (tu/point 5. 5. 0.) co/white)]
        world (wo/world {:objects [floor red green blue]
                         :lights lights})
        view (tr/view (-> (tu/point 0. 1. 0.) (tu/add (-> (tu/vector 1. 0.1 0.) (tu/mul 4.))))
                      (tu/point 0. 1. 0.)
                      (tu/vector 0. 1. 0.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 2.)
                        :focal-length 4.
                        :aperture 0.02
                        :transform view
                        :oversampling 2
                        :blur-oversampling 4
                        :parallel-depth 8})]
        ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    ;; print the PPM file
    (spit
     "./examples/img/blur-example.ppm"
     (prof/profile
      ; {:event :alloc}
      (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))))
