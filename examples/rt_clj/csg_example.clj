; # Example: CSG shapes

; {:nextjournal.clerk/visibility {:code :hide :result :hide}}
; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)

(ns rt-clj.csg-example
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

(let [filename "examples/img/csg-spheres-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

(let [filename "examples/img/csg-cube-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

(let [filename "examples/img/csg-lens-example.png"]
  (when (.exists (io/file filename))
    (clerk/image filename)))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn -main []
  (let [sp1 (os/sphere
             {:color (co/color 0.9 0.1 0.9)}
             (tr/scaling 3. 3. 3.))
        sp2 (os/sphere
             {:color (co/color 0.9 0.9 0.1)}
             (->> (tr/scaling 2. 2. 2.)
                  (ma/mul (tr/translation 2. 1. 2.))))
        shape (os/csg-shape :difference sp1 sp2)
        light (li/point-light (tu/point 5. 10. 10.)
                              (co/color 1. 1. 1.))
        world (wo/world [shape] [light])
        view (tr/view (tu/point 8. 12. 4.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera (* resolution 150) (* resolution 100) (/ Math/PI 3) view)]
    (spit "./examples/img/csg-spheres-example.ppm"
          (clojure.string/join
           "\n"
           (ca/ppm-rows (cm/render cam world)))))

  (let [cyl1 (-> (os/cylinder -2. 2. true)
                 (os/with-material {:color (co/color 1. 0.2 0.2)}))
        cyl2 (os/cylinder
              -2. 2. true
              {:color (co/color 1. 0.2 0.2)}
              (tr/rotation-z (/ Math/PI 2.)))
        cyl3 (os/cylinder
              -2. 2. true
              {:color (co/color 1. 0.2 0.2)}
              (tr/rotation-x (/ Math/PI 2.)))
        cyl (os/csg-shape :union cyl1
                          (os/csg-shape :union cyl2 cyl3))
        cube (os/cube
              {:color (co/color 0.2 1. 0.2)}
              (tr/scaling 1.9 1.9 1.9))
        sphere (os/sphere
                {:color (co/color 0.2 0.2 1.)}
                (tr/scaling 2.6 2.6 2.6))
        shape (os/csg-shape
               :intersection sphere
               (os/csg-shape
                :difference cube cyl))
        light (li/point-light (tu/point 5. 10. 10.)
                              (co/color 1. 1. 1.))
        world (wo/world [shape] [light])
        view (tr/view (tu/point 8. 12. 4.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera (* resolution 150) (* resolution 100) (/ Math/PI 3) view)]
         ; cam-crit (cm/camera 1 1 (/ Math/PI 3) view)]
    ; (println "Start profiling...")
    ; (criterium/quick-bench
    ;  (clojure.string/join "\n" (ca/ppm-rows (cm/render cam-crit world))))
    (spit
     "./examples/img/csg-cube-example.ppm"
     (prof/profile
      (clojure.string/join
       "\n"
       (ca/ppm-rows (cm/render cam world))))))

  (let [walls (os/cube
               {:color (co/color 0.5 0.2 0.8)}
               (->> (tr/scaling 20. 20. 20.)
                    (ma/mul (tr/translation 11 11 11))))
        cub1 (os/cube
              {:color (co/color 0.3 0.9 0.3)}
              (tr/scaling 2. 2. 2.))
        cub2 (os/cube
              {:color (co/color 0.9 0.9 0.3)}
              (->> (tr/scaling 2. 2. 2.)
                   (ma/mul (tr/rotation-z (/ Math/PI 4.)))
                   (ma/mul (tr/rotation-x (/ Math/PI 4.)))
                   (ma/mul (tr/translation 0. 3. 0.))))
        cub (os/csg-shape
             :difference cub1 cub2
             nil ;; default-material
             (->> (tr/rotation-z (- (/ Math/PI 7.)))
                  (ma/mul (tr/rotation-x (/ Math/PI 7.)))))
        sp1 (os/sphere
             {:color co/black
              :refractive-index 1.5
              :shininess 300
              :transparency 1.
              :shadow? false}
             (tr/scaling 0.5 1. 1.))
        sp2 (os/sphere
             {:color co/black
              :refractive-index 1.5
              :shininess 300
              :transparency 1.
              :shadow? false}
             (->> (tr/scaling 0.5 1. 1.)
                  (ma/mul (tr/translation 0.4 0. 0.))))
        lens (os/csg-shape
              :intersection sp1 sp2
              nil ;; default material
              (->> (tr/scaling 2. 2. 2.)
                   (ma/mul (tr/rotation-z (/ Math/PI 5.)))
                   (ma/mul (tr/translation 3.5 3.5 0.))))
        sp3 (os/sphere
             {:color co/black
              :reflective 1.
              :shininess 300
              :transparency 1.
              :shadow? false}
             (tr/scaling 1. 2. 2.))
        sp4 (os/sphere
             {:color co/black
              :reflective 1.
              :shininess 300}
             (->> (tr/scaling 1. 2. 2.)
                  (ma/mul (tr/translation 0.1 0. 0.))))

        mirror (os/csg-shape
                :difference sp3 sp4
                nil ;; default-material
                (->> (tr/scaling 1.5 1.5 1.5)
                     (ma/mul (tr/rotation-z (/ Math/PI 7.)))
                     (ma/mul (tr/rotation-y (/ Math/PI 7.)))
                     (ma/mul (tr/translation -3. 2. 3.))))
        l1 (li/point-light (tu/point 5. 10. 10.)
                           (co/color 0.6 0.6 0.6))
        l2 (li/point-light (tu/point -5. 10. 10.)
                           (co/color 0.4 0.4 0.4))
        world (wo/world [walls cub mirror lens] [l1 l2])
        view (tr/view (tu/point 8. 10. 4.)
                      (tu/point 0. 0. 0.)
                      (tu/vector 0. 0. 1.))
        resolution 4
        cam (cm/camera (* resolution 150) (* resolution 100) (/ Math/PI 3) view)]
    (spit "./examples/img/csg-lens-example.ppm"
          (clojure.string/join
           "\n"
           (ca/ppm-rows (cm/render cam world))))))
