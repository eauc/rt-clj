; # Examples: OBJ files

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.obj-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
            ; [criterium.core :as criterium]
            [clj-async-profiler.core :as prof]
            [nextjournal.clerk :as clerk]
            [rt-clj.cameras :as cm]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.obj-files :as obj-files]
            [rt-clj.lights :as li]
            [rt-clj.objects :as os]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]
            [rt-clj.worlds :as wo])
  (:import java.lang.Math))

(let [filename "examples/img/obj-teapot-low-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

(let [filename "examples/img/obj-teapot-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [floor (os/plane
               {:color (co/color 0.3 0.1 0.3)
                :reflective 1
                :shininess 300}
               (tr/rotation-x (/ Math/PI 2.)))
        teapot (with-open [rdr (clojure.java.io/reader "./examples/obj/teapot-low.obj")]
                 (obj-files/parse-lines
                  (line-seq rdr)
                  {:color (co/color 0.6 0.6 0.6)
                   :reflective 1.
                   :shininess 300}))
        light (li/point-light (tu/point 50. 100. 100.)
                              (co/color 1. 1. 1.))
        world (wo/world [floor (:group teapot)] [light])
        view (tr/view (tu/point 20. 40. 20.)
                      (tu/point 0. 0. 5.)
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
    (spit
     "./examples/img/obj-teapot-low-example.ppm"
     (prof/profile
      (clojure.string/join
       "\n"
       (ca/ppm-rows (cm/render cam world))))))

  (let [floor (os/plane
               {:color (co/color 0.3 0.1 0.3)
                :reflective 1
                :shininess 300}
               (tr/rotation-x (/ Math/PI 2.)))
        teapot (with-open [rdr (clojure.java.io/reader "./examples/obj/teapot.obj")]
                 (obj-files/parse-lines
                  (line-seq rdr)
                  {:color (co/color 0.6 0.6 0.6)
                   :reflective 1.
                   :shininess 300}))
        light (li/point-light (tu/point 50. 100. 100.)
                              (co/color 1. 1. 1.))
        world (wo/world [floor (:group teapot)] [light])
        view (tr/view (tu/point 20. 40. 20.)
                      (tu/point 0. 0. 5.)
                      (tu/vector 0. 0. 1.))
        resolution 8
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
    (spit "./examples/img/obj-teapot-example.ppm"
          (clojure.string/join
           "\n"
           (ca/ppm-rows (cm/render cam world {:depth 2}))))))
