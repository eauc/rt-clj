(ns rt-clj.shapes.groups-test
  (:require [clojure.test :refer [deftest is testing]]
            [rt-clj.shapes.groups :refer :all]
            [rt-clj.bounds :as bd]
            [rt-clj.object-protocol :as o]
            [rt-clj.objects :as os]
            [rt-clj.rays :as r]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]))

(deftest groups-test

  (testing "Intersecting a ray with an empty group"
    (is (= []
           (sh/local-intersect
            (group [])
            (r/ray (t/point 0. 0. 0.) (t/vector 0. 0. 1.))
            {:bounds bd/infinite}))))

  (testing "Intersecting a ray with a nonempty group"
    (let [s1 (os/sphere)
          s2 (-> (os/sphere) (os/with-transform (tr/translation 0. 0. -3.)))
          s3 (-> (os/sphere) (os/with-transform (tr/translation 5. 0. 0.)))
          g (group [s1 s2 s3])
          r (r/ray (t/point 0. 0. -5.) (t/vector 0. 0. 1.))]
      (is (= [(nth (:children g) 1)
              (nth (:children g) 1)
              (nth (:children g) 0)
              (nth (:children g) 0)]
             (mapv :object (sh/local-intersect g r {:bounds bd/infinite}))))))

  (testing "Intersecting a transformed group"
    (let [g (-> (os/group [(-> (os/sphere) (os/with-transform (tr/translation 5. 0. 0.)))])
                (os/with-transform (tr/scaling 2. 2. 2.)))
          r (r/ray (t/point 10. 0. -10.) (t/vector 0. 0. 1.))]
      (is (= 2
             (count (o/intersect g r)))))))
