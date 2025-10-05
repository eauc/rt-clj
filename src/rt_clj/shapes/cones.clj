; # Cones

(ns rt-clj.shapes.cones
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Bounds

(defn- local-bounds
  [{:keys [^double minimum ^double maximum]}]
  (let [max-abs (max (Math/abs minimum)
                     (Math/abs maximum))]
    {:min (t/point (- max-abs) minimum (- max-abs))
     :max (t/point max-abs maximum max-abs)}))

; ## Intersections

; The same as cylinders, except the radius of the cone is the absolute value of `y`.

(defn- check-cap
  [{:keys [origin direction]} ^double t ^double y]
  (let [x (+ (t/x origin) (* t (t/x direction)))
        z (+ (t/z origin) (* t (t/z direction)))]
    (>= (Math/abs y) (+ (Math/pow x 2.)
                        (Math/pow z 2.)))))

(defn- intersect-caps
  [{:keys [closed? ^double minimum ^double maximum]}
   {:keys [origin direction] :as ray}
   object]
  (if (or (not closed?)
          (t/close? 0. (t/y direction)))
    []
    (let [t-min (/ (- minimum (t/y origin)) (t/y direction))
          t-max (/ (- maximum (t/y origin)) (t/y direction))
          cap-min? (check-cap ray t-min minimum)
          cap-max? (check-cap ray t-max maximum)]
      (cond
        (and cap-min? cap-max?) [(i/intersection t-min object) (i/intersection t-max object)]
        cap-min? [(i/intersection t-min object)]
        cap-max? [(i/intersection t-max object)]
        :else []))))

; The same as cylinders, except the formula for `a,b,c`.

; Also, the ray misses the cone when a & b are zero (not only a).

; The distance of the hit when a = 0 but b != 0 (the ray is parallel to one half of the cone but intersect the other) is slightly different.

(defn- intersect-sides
  [{:keys [minimum maximum]}
   {:keys [direction origin]}
   object]
  (let [a (- (+ (Math/pow (t/x direction) 2.)
                (Math/pow (t/z direction) 2.))
             (Math/pow (t/y direction) 2.))
        b (- (+ (* 2 (t/x origin) (t/x direction))
                (* 2 (t/z origin) (t/z direction)))
             (* 2 (t/y origin) (t/y direction)))]
    (if (and (t/close? a 0.)
             (t/close? b 0.))
      []
      (let [c (- (+ (Math/pow (t/x origin) 2.)
                    (Math/pow (t/z origin) 2.))
                 (Math/pow (t/y origin) 2.))
            disc (- (Math/pow b 2.) (* 4. a c))]
        (if (< disc 0.)
          []
          (if (t/close? a 0.)
            [(i/intersection (- (/ c (* 2. b))) object)]
            (let [disc-sqrt (Math/sqrt disc)
                  t0 (/ (- 0. b disc-sqrt) (* 2. a))
                  t1 (/ (+ (- 0. b) disc-sqrt) (* 2. a))
                  y0 (+ (t/y origin) (* t0 (t/y direction)))
                  y1 (+ (t/y origin) (* t1 (t/y direction)))
                  y0-in-bounds (< minimum y0 maximum)
                  y1-in-bounds (< minimum y1 maximum)]
              (cond
                (and y0-in-bounds y1-in-bounds) [(i/intersection t0 object) (i/intersection t1 object)]
                y0-in-bounds [(i/intersection t0 object)]
                y1-in-bounds [(i/intersection t1 object)]
                :else []))))))))

(defn- local-intersect
  [cne ray object]
  (concat
   (intersect-sides cne ray object)
   (intersect-caps cne ray object)))

; ## Normal

; The same as cylinders, except the normal as an `y` component.

(defn local-normal
  [{:keys [^double minimum ^double maximum]} point]
  (let [d (+ (Math/pow (t/x point) 2.)
             (Math/pow (t/z point) 2.))]
    (cond
      (and (< d 1) (>= (t/y point) (- maximum (double t/epsilon)))) (t/vector 0. 1. 0.)
      (and (< d 1) (<= (t/y point) (+ minimum (double t/epsilon)))) (t/vector 0. -1. 0.)
      :else (let [y (Math/sqrt (+ (Math/pow (t/x point) 2.)
                                  (Math/pow (t/z point) 2.)))]
              (t/vector (t/x point)
                        (if (< (t/y point) 0.) y (- y))
                        (t/z point))))))

; ## Creation

(defrecord Cone [minimum maximum closed?]
  sh/Shape
  (local-bounds [cne]
    (local-bounds cne))
  (local-intersect [cne ray object]
    (local-intersect cne ray object))
  (local-normal [cne point _]
    (local-normal cne point)))

(defn cone
  ([minimum maximum closed?]
   (->Cone minimum maximum closed?))
  ([]
   (->Cone (- (double t/infinity)) t/infinity false)))
