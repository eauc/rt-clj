(ns rt-clj.objects-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.objects :refer :all]
            [rt-clj.object-protocol :as o]
            [rt-clj.matrices :as m]
            [rt-clj.materials :as mr]
            ; [rt-clj.groups :as gr]
            [rt-clj.rays :as r]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]))

(deftest objects-test

  (testing "An object's default transformation"
    (is (m/eq? (m/id 4)
               (:transform (sphere)))))

  (testing "Changing an object's transformation"
    (let [t (tr/translation 2. 3. 4.)
          s (-> (sphere) (with-transform t))]
      (is (m/eq? t
                 (:transform s)))))

  (testing "An object has a default material"
    (is (= mr/default-material
           (:material (sphere)))))

  (testing "Intersecting a scaled object with a ray"
    (let [ra (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          s (-> (sphere) (with-transform (tr/scaling 2. 2. 2.)))
          xs (o/intersect s ra)]
      (is (= 2
             (count xs)))
      (is (= 3.
             (:t (first xs))))
      (is (= 7.
             (:t (second xs))))))

  (testing "Intersecting a translated object with a ray"
    (let [ra (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))
          s (-> (sphere) (with-transform (tr/translation 5. 0. 0.)))
          xs (o/intersect s ra)]
      (is (= 0
             (count xs)))))

  ; (testing "Converting a point from world to object space"
  ;   (let [sphere (sphere (tr/translation 5. 0. 0.))
  ;         g2 (group (tr/scaling 2. 2. 2.) [sphere])
  ;         g1 (group (tr/rotation-y (/ Math/PI 2.)) [g2])]
  ;     (is (t/eq? (t/point 0. 0. -1.)
  ;                (world->object (-> g1 :children first :children first)
  ;                               (t/point -2. 0. -10.))))))
  ;
  ; (testing "Converting a normal from object to world space"
  ;   (let [sphere (sphere (tr/translation 5. 0. 0.))
  ;         g2 (group (tr/scaling 1. 2. 3.) [sphere])
  ;         g1 (group (tr/rotation-y (/ Math/PI 2.)) [g2])
  ;         sqrt3-on3 (/ (Math/sqrt 3.) 3.)]
  ;     (is (t/eq? (t/vector 0.285714 0.428571 -0.857142)
  ;                (object->world (-> g1 :children first :children first)
  ;                               (t/vector sqrt3-on3 sqrt3-on3 sqrt3-on3))))))
  ;
  ; (testing "Finding the normal on a child object"
  ;   (let [sphere (sphere (tr/translation 5. 0. 0.))
  ;         g2 (group (tr/scaling 1. 2. 3.) [sphere])
  ;         g1 (group (tr/rotation-y (/ Math/PI 2.)) [g2])]
  ;     (is (t/eq? (t/vector 0.285703 0.428543 -0.857160)
  ;                (normal (-> g1 :children first :children first)
  ;                        (t/point 1.7321 1.1547 -5.5774)
  ;                        {}))))))

  (testing "Computing the normal on a translated sphere"
    (is (t/eq? (t/vector 0. 0.70711 -0.70711)
               (o/normal (-> (sphere) (with-transform (tr/translation 0. 1. 0.)))
                         (t/point 0, 1.70711, -0.70711)
                         {}))))

  (testing "Computing the normal on a scaled sphere"
    (is (t/eq? (t/vector 0. 0.97014 -0.24254)
               (o/normal (-> (sphere) (with-transform (tr/scaling 1. 0.5 1.)))
                         (t/point 0, 0.70711, -0.70711)
                         {})))))
