; # Examples: patterns

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.patterns-example
  {:nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [clojure.java.io :as io]
            [clojure.string]
            [nextjournal.clerk :as clerk]
            [rt-clj.cameras :as cm]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.lights :as li]
            [rt-clj.matrices :as ma]
            [rt-clj.objects :as os]
            [rt-clj.patterns :as pt]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as tu]
            [rt-clj.worlds :as wo])
  (:import java.lang.Math))

; ## Stripes

(let [filename "examples/img/patterns-stripes-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

; ## Gradient

(let [filename "examples/img/patterns-gradient-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

; ## Rings

(let [filename "examples/img/patterns-rings-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

; ## Checker

(let [filename "examples/img/patterns-checker-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  ;; stripes
  (let [stripes (pt/stripes
                 (co/color 0. 0.8 0.3) co/white
                 (tr/scaling 0.5 0.5 0.5))
        material {:pattern stripes}
        sphere (os/sphere material (tr/scaling 4. 4. 4.))
        floor (os/plane
               material
               (ma/mul (tr/translation 0. 0. -10.)
                       (tr/rotation-x (/ Math/PI 2))))
        light (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        world (wo/world [floor sphere] [light])
        view (tr/view (tu/point 7. 10. 5.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
    (spit
     "./examples/img/patterns-stripes-example.ppm"
     (clojure.string/join
      "\n" (ca/ppm-rows (cm/render cam world)))))

  (let [gradient (pt/gradient
                  (co/color 1. 0. 0.) (co/color 0. 0. 1.)
                  (ma/mul (tr/translation 1. 0. 0.)
                          (tr/scaling 2. 2. 2.)))
        material {:pattern gradient}
        sphere (os/sphere material (tr/scaling 4. 4. 4.))
        floor (os/plane
               material
               (ma/mul (tr/translation 0. 0. -10.)
                       (tr/rotation-x (/ Math/PI 2))))
        light (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        world (wo/world [floor sphere] [light])
        view (tr/view (tu/point 7. 10. 5.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
    (spit "./examples/img/patterns-gradient-example.ppm"
          (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))

  (let [rings (pt/rings
               (co/color 0. 0.8 0.3) co/white
               (tr/scaling 0.33 0.33 0.33))
        material {:pattern rings}
        sphere (os/sphere material (tr/scaling 4. 4. 4.))
        light (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        floor (os/plane
               material
               (ma/mul (tr/translation 0. 0. -10.)
                       (tr/rotation-x (/ Math/PI 2))))
        world (wo/world [floor sphere] [light])
        view (tr/view (tu/point 12. 7. 5.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
    (spit "./examples/img/patterns-rings-example.ppm"
          (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world)))))

  (let [checker (pt/checker
                 (co/color 0. 0.3 0.8) co/white)
        material {:pattern checker}
        sphere (os/sphere material (tr/scaling 4. 4. 4.))
        floor (os/plane
               material
               (ma/mul (tr/translation 0. 0. -10.)
                       (tr/rotation-x (/ Math/PI 2))))
        light (li/point-light (tu/point 10. 10. 10.) (co/color 1. 1. 1.))
        world (wo/world [floor sphere] [light])
        view (tr/view (tu/point 12. 7. 5.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera {:hsize (* resolution 150)
                        :vsize (* resolution 100)
                        :fov (/ Math/PI 3)
                        :transform view})]
    (spit "./examples/img/patterns-checker-example.ppm"
          (clojure.string/join "\n" (ca/ppm-rows (cm/render cam world))))))
