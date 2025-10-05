(ns rt-clj.materials-test
  (:import java.lang.Math)
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.materials :refer :all]
            [rt-clj.colors :as c]
            [rt-clj.lights :as l]
            [rt-clj.objects :as os]
            [rt-clj.patterns :as pt]
            [rt-clj.tuples :as t]))

(deftest materials-test
  (testing "The default material"
    (let [m default-material]
      (is (= c/white
             (:color m)))
      (is (= 0.1
             (:ambient m)))
      (is (= 0.9
             (:diffuse m)))
      (is (= 0.9
             (:specular m)))
      (is (= 200
             (:shininess m)))))

  (let [m default-material
        position (t/point 0. 0. 0.)
        pi-4 (/ (Math/sqrt 2) 2)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev (t/vector 0. 0. -1.)
            normalv (t/vector 0. 0. -1.)
            light (l/point-light (t/point 0. 0. -10.) (c/color 1. 1. 1.))]
        (is (t/eq? (c/color 1.9 1.9 1.9)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv)))))

    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let [eyev (t/vector 0. pi-4 (- 0 pi-4))
            normalv (t/vector 0. 0. -1.)
            light (l/point-light (t/point 0. 0. -10.) (c/color 1. 1. 1.))]
        (is (t/eq? (c/color 1. 1. 1.)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv)))))

    (testing "Lighting with eye opposite surface, light offset 45°"
      (let [eyev (t/vector 0. 0. -1.)
            normalv (t/vector 0. 0. -1.)
            light (l/point-light (t/point 0. 10. -10.) (c/color 1. 1. 1.))]
        (is (t/eq? (c/color 0.7364 0.7364 0.7364)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv)))))

    (testing "Lighting with eye in the path of the reflection vector"
      (let [eyev (t/vector 0. (- 0 pi-4) (- 0 pi-4))
            normalv (t/vector 0. 0. -1.)
            light (l/point-light (t/point 0. 10. -10.) (c/color 1. 1. 1.))]
        (is (t/eq? (c/color 1.6364 1.6364 1.6364)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv)))))

    (testing "Lighting with the light behind the surface"
      (let [eyev (t/vector 0. 0. -1.)
            normalv (t/vector 0. 0. -1.)
            light (l/point-light (t/point 0. 0. 10.) (c/color 1. 1. 1.))]
        (is (t/eq? (c/color 0.1 0.1 0.1)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv)))))

    (testing "Lighting with the surface in shadow"
      (let [eyev (t/vector 0. 0. -1.)
            normalv (t/vector 0. 0. -1.)
            ;; lights in shadow have :intensity = black
            light (l/point-light (t/point 0. 0. -10.) c/black)]
        (is (t/eq? (c/color 0.1 0.1 0.1)
                   (lighting
                    m (os/sphere)
                    c/white [light]
                    position eyev normalv))))))

  (testing "Lighting with a pattern applied"
    (let [mat (-> default-material
                  (assoc :pattern (pt/stripes c/white c/black)
                         :ambient 1.
                         :diffuse 0.
                         :specular 0.))
          eyev (t/vector 0. 0. -1.)
          normalv (t/vector 0. 0. -1.)
          light (l/point-light (t/point 0. 0. -10.) (c/color 1. 1. 1.))]
      (is (t/eq? c/white
                 (lighting
                  mat (os/sphere)
                  c/white [light]
                  (t/point 0.9 0. 0.) eyev normalv)))
      (is (t/eq? c/black
                 (lighting
                  mat (os/sphere)
                  c/white [light]
                  (t/point 1.1 0. 0.) eyev normalv))))))
