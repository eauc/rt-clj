; # Example: groups

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.groups-example
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

(let [filename "examples/img/groups-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [mat {:color (co/color 0.8 0.2 0.8)}
        cyl (os/cylinder
             -1. 1. false
             mat
             (tr/scaling 0.5 1. 0.5))
        sph (os/sphere
             mat
             (->> (tr/scaling 0.5 0.5 0.5)
                  (ma/mul (tr/translation 0. 1. 0))))
        hex-side (fn hex-side [^double n]
                   (os/group
                    [cyl sph]
                    mat
                    (->> (tr/translation 1.732050 0. 0.)
                         (ma/mul (tr/rotation-z (* n (/ Math/PI 3.)))))))
        hex (fn hex [transform]
              (os/group
               [(hex-side 0.)
                (hex-side 1.)
                (hex-side 2.)
                (hex-side 3.)
                (hex-side 4.)
                (hex-side 5.)]
               mat
               transform))
        grps [(hex (ma/id 4))]
              ; (hex (->> (tr/rotation-x (* 3. (/ Math/PI 4.)))
              ;           (ma/mul (tr/translation 2.5 0. 0.))))
              ; (hex (->> (tr/rotation-x (/ Math/PI 4.))
              ;           (ma/mul (tr/translation -2.5 0. 0.))))]
        light (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        world (wo/world grps [light])
        view (tr/view (tu/point 5. 10. 6.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera (* 150 resolution) (* 100 resolution) (/ Math/PI 3) view)]
        ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    (spit
     "./examples/img/groups-example.ppm"
     (prof/profile
      (clojure.string/join
       "\n" (ca/ppm-rows (cm/render cam world)))))))
