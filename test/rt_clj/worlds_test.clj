(ns rt-clj.worlds-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.worlds :refer :all]
            [rt-clj.colors :as c]
            [rt-clj.intersections :as i]
            [rt-clj.lights :as l]
            [rt-clj.materials :as m]
            [rt-clj.objects :as os]
            [rt-clj.patterns :as pt]
            [rt-clj.rays :as r]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]))

(deftest worlds-test
  (testing "Creating a world"
    (let [w (world)]
      (is (= []
             (:objects w)))
      (is (= []
             (:lights w)))))

  (testing "Intersect a world with a ray"
    (let [w (default-world)
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          xs (intersect w ray)]
      (is (= 4
             (count xs)))
      (is (= 4.
             (:t (first xs))))
      (is (= 4.5
             (:t (second xs))))
      (is (= 5.5
             (:t (nth xs 2))))
      (is (= 6.
             (:t (nth xs 3))))))

  (testing "Shading an intersection"
    (let [w (default-world)
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          shape (first (:objects w))
          int (i/intersection 4. shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? (c/color 0.38066, 0.47583, 0.2855)
                 (shade-hit w int comps 1)))))

  (testing "Shading an intersection from the inside"
    (let [w (assoc (default-world)
                   :lights [(l/point-light (t/point 0. 0.25 0.) (c/color 1. 1. 1.))])
          ray (r/ray t/origin (t/vector 0. 0. 1.))
          shape (second (:objects w))
          int (i/intersection 0.5 shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? (c/color 0.90498 0.90498 0.90498)
                 (shade-hit w int comps 1)))))

  (testing "Shading an intersection in shadow"
    (let [s2 (-> (os/sphere) (os/with-transform (tr/translation 0. 0. 10.)))
          w (world [(os/sphere) s2]
                   [(l/point-light (t/point 0. 0. -10.) (c/color 1. 1. 1.))])
          ray (r/ray (t/point 0. 0. 5.) (t/vector 0. 0. 1.))
          int (i/intersection 4. s2)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? (c/color 0.1, 0.1, 0.1)
                 (shade-hit w int comps 1)))))

  (testing "Shading an intersection with a reflective material"
    (let [shape (os/plane
                 (-> m/default-material (assoc :reflective 0.5))
                 (tr/translation 0. -1. 0.))
          w (-> (default-world) (update :objects #(conj % shape)))
          a (/ (Math/sqrt 2) 2)
          ray (r/ray (t/point 0. 0. -3.) (t/vector 0. (- a) a))
          int (i/intersection (Math/sqrt 2) shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? (c/color 0.876757 0.924340 0.829174)
                 (shade-hit w int comps 1)))))

  (testing "The color when a ray misses"
    (let [w (default-world)
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 1. 0.))]
      (is (t/eq? (c/color 0. 0. 0.0)
                 (color w ray 1)))))

  (testing "The color when a ray hits"
    (let [w (default-world)
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))]
      (is (t/eq? (c/color 0.38066 0.47583 0.2855)
                 (color w ray 1)))))

  (testing "The color with an intersection behind the ray"
    (let [w (-> (default-world)
                (assoc-in [:objects 0 :material :ambient] 1)
                (update-in [:objects 1 :material] assoc :ambient 1 :diffuse 0))
          ray (r/ray (t/point 0. 0. -0.75) (t/vector 0. 0. 1.))]
      (is (t/eq? (get-in w [:objects 1 :material :color])
                 (color w ray 1)))))

  (let [light (get-in (default-world) [:lights 0])]
    (testing "There is no shadow when nothing is collinear with point and light"
      (is (= false
             (shadowed? (default-world) (t/point 0. 10. 0.) light))))

    (testing "The shadow when an object is between the point and the light"
      (is (= true
             (shadowed? (default-world) (t/point 10. -10. 10.) light))))

    (testing "Objects can opt out of shadow calculation"
      (is (= false
             (shadowed? (world
                         (-> (:objects (default-world))
                             (assoc-in [0 :material :shadow?] false)
                             (assoc-in [1 :material :shadow?] false))
                         (:lights (default-world)))
                        (t/point 10. -10. 10.) light))))

    (testing "There is no shadow when an object is behind the light"
      (is (= false
             (shadowed? (default-world) (t/point -20. 20. -20.) light))))

    (testing "There is no shadow when an object is behind the point"
      (is (= false
             (shadowed? (default-world) (t/point -2. 2. -2.) light)))))

  (testing "The reflected color for a nonreflective material"
    (let [w (-> (default-world) (assoc-in [:objects 1 :material :ambient] 1.))
          ray (r/ray (t/point 0. 0. 0.) (t/vector 0. 0. 1))
          shape (get-in w [:objects 1])
          int (i/intersection 1. shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? c/black
                 (reflected-color w int comps 1)))))

  (testing "The reflected color for a reflective material"
    (let [shape (os/plane
                 (-> m/default-material (assoc :reflective 0.5))
                 (tr/translation 0. -1. 0.))
          w (-> (default-world) (update :objects #(conj % shape)))
          a (/ (Math/sqrt 2) 2)
          ray (r/ray (t/point 0. 0. -3.) (t/vector 0. (- a) a))
          int (i/intersection (Math/sqrt 2) shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? (c/color 0.190332 0.237915 0.142749)
                 (reflected-color w int comps 1)))))

  (testing "The reflected color at the maximum recursive depth"
    (let [shape (os/plane
                 (-> m/default-material (assoc :reflective 0.5))
                 (tr/translation 0. -1. 0.))
          w (-> (default-world) (update :objects #(conj % shape)))
          a (/ (Math/sqrt 2) 2)
          ray (r/ray (t/point 0. 0. -3.) (t/vector 0. (- a) a))
          int (i/intersection (Math/sqrt 2) shape)
          comps (i/prepare-hit int ray [int])]
      (is (t/eq? c/black
                 (reflected-color w int comps 0)))))

  (testing "The refracted color with an opaque surface"
    (let [w (default-world)
          shape (get-in w [:objects 0])
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          xs [(i/intersection 4. shape)
              (i/intersection 6. shape)]
          hit (first xs)
          comps (i/prepare-hit (first xs) ray xs)]
      (is (t/eq? c/black
                 (first (refracted-color w hit comps 5))))))

  (testing "The refracted color at the maximum recursive depth"
    (let [w (-> (default-world)
                (assoc-in [:objects 0 :material :transparency] 1.)
                (assoc-in [:objects 0 :material :refractive-index] 1.5))
          shape (get-in w [:objects 0])
          ray (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          xs [(i/intersection 4. shape)
              (i/intersection 6. shape)]
          hit (first xs)
          comps (i/prepare-hit (first xs) ray xs)]
      (is (t/eq? c/black
                 (first (refracted-color w hit comps 0))))))

  (testing "The refracted color under total internal reflection"
    (let [w (-> (default-world)
                (assoc-in [:objects 0 :material :transparency] 1.)
                (assoc-in [:objects 0 :material :refractive-index] 1.5))
          shape (get-in w [:objects 0])
          ray (r/ray (t/point 0. 0. (/ (Math/sqrt 2.) 2.)) (t/vector 0. 1. 0.))
          xs [(i/intersection (- (/ (Math/sqrt 2.) 2.)) shape)
              (i/intersection (/ (Math/sqrt 2.) 2.) shape)]
          hit (nth xs 1)
          comps (i/prepare-hit (nth xs 1) ray xs)]
      (is (t/eq? c/black
                 (first (refracted-color w hit comps 5))))))

  (testing "The refracted color with a refracted ray"
    (let [w (-> (default-world)
                (assoc-in [:objects 0 :material :ambient] 1.)
                (assoc-in [:objects 0 :material :pattern] (pt/test-pattern))
                (assoc-in [:objects 1 :material :transparency] 1.)
                (assoc-in [:objects 1 :material :refractive-index] 1.5))
          A (get-in w [:objects 0])
          B (get-in w [:objects 1])
          ray (r/ray (t/point 0. 0. 0.1) (t/vector 0. 1. 0.))
          xs [(i/intersection -0.9899 A)
              (i/intersection -0.4899 B)
              (i/intersection 0.4899 B)
              (i/intersection 0.9899 A)]
          hit (nth xs 2)
          comps (i/prepare-hit (nth xs 2) ray xs)]
      (is (t/eq? (c/color 0. 0.998874 0.047218)
                 (first (refracted-color w hit comps 5))))))

  (testing "shade-hit with a transparent material"
    (let [floor (os/plane
                 (assoc m/default-material
                        :transparency 0.5
                        :refractive-index 1.5)
                 (tr/translation 0. -1. 0.))
          ball (os/sphere
                (assoc m/default-material
                       :ambient 0.5
                       :color (c/color 1. 0. 0.))
                (tr/translation 0. -3.5 -0.5))
          w (world
             (-> (:objects (default-world))
                 (conj floor)
                 (conj ball))
             (:lights (default-world)))
          sqrt2_on2 (/ (Math/sqrt 2.) 2.)
          ray (r/ray (t/point 0. 0. -3.) (t/vector 0. (- sqrt2_on2) sqrt2_on2))
          xs [(i/intersection (Math/sqrt 2.) floor)]
          comps (i/prepare-hit (first xs) ray xs)]
      (is (t/eq? (c/color 0.936425 0.686425 0.686425)
                 (shade-hit w (first xs) comps 5)))))

  (testing "shade-hit with a reflective, transparent material"
    (let [sqrt2_on2 (/ (Math/sqrt 2.) 2.)
          floor (os/plane
                 (assoc m/default-material
                        :reflective 0.5
                        :transparency 0.5
                        :refractive-index 1.5)
                 (tr/translation 0. -1. 0.))
          ball (os/sphere
                (assoc m/default-material
                       :color (c/color 1. 0. 0.)
                       :ambient 0.5)
                (tr/translation 0. -3.5 -0.5))
          w (world
             (-> (:objects (default-world))
                 (conj floor)
                 (conj ball))
             (:lights (default-world)))
          ray (r/ray (t/point 0. 0. -3.) (t/vector 0. (- sqrt2_on2) sqrt2_on2))
          xs [(i/intersection (Math/sqrt 2.) floor)]
          comps (i/prepare-hit (first xs) ray xs)]
      (is (t/eq? (c/color 0.933915 0.696434 0.692430)
                 (shade-hit w (first xs) comps 5))))))
