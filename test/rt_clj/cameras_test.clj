(ns rt-clj.cameras-test
  (:require [clojure.test :refer :all]
            [rt-clj.cameras :refer :all]
            [rt-clj.canvas :as ca]
            [rt-clj.colors :as co]
            [rt-clj.matrices :as m]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]
            [rt-clj.worlds :as w]))

(deftest cameras-test
  (testing "Constructing a camera"
    (let [cam (camera {:hsize 160 :vsize 120 :fov (/ Math/PI 2)})]
      (is (= 160
             (:hsize cam)))
      (is (= 120
             (:vsize cam)))
      (is (= (/ Math/PI 2)
             (:fov cam)))
      (is (m/eq? (m/id 4)
             (:transform cam)))))

  (testing "The pixel size for a horizontal canvas"
    (is (t/close? 0.01
                  (:pixel-size (camera {:hsize 200. :vsize 125. :fov (/ Math/PI 2)})))))
  
  (testing "The pixel size for a horizontal canvas"
    (is (t/close? 0.01
                  (:pixel-size (camera {:hsize 125. :vsize 200. :fov (/ Math/PI 2)})))))

  (testing "Construct a ray through the center of the canvas"
    (let [cam (camera {:hsize 201. :vsize 101 :fov (/ Math/PI 2)})
          [ray] (pixel-rays cam 100 50)]
      (is (t/eq? (t/point 0. 0. 0.)
             (:origin ray)))
      (is (t/eq? (t/vector 0. 0. -1.)
                 (:direction ray)))))
  
  (testing "Construct a ray through a corner of the canvas"
    (let [cam (camera {:hsize 201. :vsize 101. :fov (/ Math/PI 2)})
          [ray] (pixel-rays cam 0 0)]
      (is (t/eq? (t/point 0. 0. 0.)
             (:origin ray)))
      (is (t/eq? (t/vector 0.66519 0.33259 -0.66851)
                 (:direction ray)))))
  
  (testing "Construct a ray when the camera is transformed"
    (let [cam (camera {:hsize 201 :vsize 101 :fov (/ Math/PI 2)
                       :transform (m/mul (tr/rotation-y (/ Math/PI 4))
                                         (tr/translation 0. -2. 5.))})
          [ray] (pixel-rays cam 100 50)]
      (is (t/eq? (t/point 0. 2. -5.)
             (:origin ray)))
      (is (t/eq? (t/vector (/ (Math/sqrt 2) 2) 0. (- (/ (Math/sqrt 2) 2)))
                 (:direction ray)))))

  (testing "Construct rays for with oversampling"
    (let [cam (camera {:hsize 201. :vsize 101 :fov (/ Math/PI 2) :oversampling 2})
          rays (pixel-rays cam 100 50)
          [r1 r2 r3 r4] rays]
      (is (= 4
             (count rays)))
      (is (t/eq? (t/point 0. 0. 0.)
                 (:origin r1)))
      (is (t/eq? (t/vector 0.002487 0.002487 -0.999993)
                 (:direction r1)))
      (is (t/eq? (t/point 0. 0. 0.)
                 (:origin r2)))
      (is (t/eq? (t/vector 0.002487 -0.002487 -0.999993)
                 (:direction r2)))
      (is (t/eq? (t/point 0. 0. 0.)
                 (:origin r3)))
      (is (t/eq? (t/vector -0.002487 0.002487 -0.999993)
                 (:direction r3)))
      (is (t/eq? (t/point 0. 0. 0.)
                 (:origin r4)))
      (is (t/eq? (t/vector -0.002487 -0.002487 -0.999993)
                 (:direction r4)))))

  (testing "Rendering a world with a camera"
    (let [world (w/default-world)
          cam (camera {:hsize 11 :vsize 11 :fov (/ Math/PI 2)
                       :transform (tr/view (t/point 0. 0. -5.)
                                           (t/point 0. 0. 0.)
                                           (t/vector 0. 1. 0.))})]
      (is (t/eq? (co/color 0.38066, 0.47583, 0.2855)
                 (ca/get-at (render cam world) 5 5))))))
