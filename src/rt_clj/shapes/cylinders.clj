; # Cylinders

(ns rt-clj.shapes.cylinders
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [rt-clj.bounds :as bd]
            [rt-clj.intersections :as i]
            [rt-clj.shape-protocol :as sh]
            [rt-clj.tuples :as t]))

; ## Bounds

(defn local-bounds
  [{:keys [minimum maximum]}]
  (bd/bounds
   (t/point -1. minimum -1.)
   (t/point 1. maximum 1.)))

(defn check-cap
  [{:keys [origin direction]} ^double t]
  (let [x (+ (t/x origin) (* t (t/x direction)))
        z (+ (t/z origin) (* t (t/z direction)))]
    (>= 1. (+ (Math/pow x 2.)
              (Math/pow z 2.)))))

; ## Intersections

; If the cylinder is closed, we need to calculate the intersections with the caps.

; `intersect-caps` checks to see if the given ray intersects the end caps of the given cylinder, and adds the points of intersection (if any) to the hits collection.

(defn intersect-caps
  [{:keys [closed? ^double minimum ^double maximum]}
   {:keys [origin direction] :as ray}
   object]
  (if (or (not closed?)
          (t/close? 0. (t/y direction)))
    []
    (let [t-min (/ (- minimum (t/y origin)) (t/y direction))
          t-max (/ (- maximum (t/y origin)) (t/y direction))
          cap-min? (check-cap ray t-min)
          cap-max? (check-cap ray t-max)]
      (cond
        (and cap-min? cap-max?) [(i/intersection t-min object) (i/intersection t-max object)]
        cap-min? [(i/intersection t-min object)]
        cap-max? [(i/intersection t-max object)]
        :else []))))

; We first calculate a pseudo-discrimant, which is negative is the ray doesn't intersect the cylinder.

; Otherwise we use it to calculate roots and the intersections.

; We also need to calculate the `y` coordinate at each intersection and check it is between `minimum` and `maximum` properties for the cylinder. If not, the intersection is not valid.

(defn- intersect-sides
  [{:keys [minimum maximum]}
   {:keys [direction origin]}
   object]
  (let [a (* (+ (Math/pow (t/x direction) 2.)
                (Math/pow (t/z direction) 2.))
             2.)]
    (if (t/close? a 0.)
      []
      (let [b (+ (* 2 (t/x origin) (t/x direction))
                 (* 2 (t/z origin) (t/z direction)))
            c (+ (Math/pow (t/x origin) 2.)
                 (Math/pow (t/z origin) 2.)
                 -1.)
            disc (- (Math/pow b 2.) (* 2. a c))]
        (if (< disc 0.)
          []
          (let [disc-sqrt (Math/sqrt disc)
                t0 (/ (- 0. b disc-sqrt) a)
                t1 (/ (+ (- 0. b) disc-sqrt) a)
                y0 (+ (t/y origin) (* t0 (t/y direction)))
                y1 (+ (t/y origin) (* t1 (t/y direction)))
                y0-in-bound (< minimum y0 maximum)
                y1-in-bound (< minimum y1 maximum)]
            (cond
              (and y0-in-bound y1-in-bound) [(i/intersection t0 object) (i/intersection t1 object)]
              y0-in-bound [(i/intersection t0 object)]
              y1-in-bound [(i/intersection t1 object)]
              :else [])))))))

(defn local-intersect
  [cyl ray object]
  (concat
   (intersect-sides cyl ray object)
   (intersect-caps cyl ray object)))

; ## Normal

; Finding the normal of a cylinder is quite easy, you just need to remove the `y` coordinate of the point on the surface.

; When the point is on one of the cylinder's cap, just return =+/-u[y]=.

(defn local-normal
  [{:keys [^double minimum ^double maximum]} point]
  (let [d (+ (Math/pow (t/x point) 2.)
             (Math/pow (t/z point) 2.))
        e (double t/epsilon)]
    (cond
      (and (< d 1) (>= (t/y point) (- maximum e))) (t/vector 0. 1. 0.)
      (and (< d 1) (<= (t/y point) (+ minimum e))) (t/vector 0. -1. 0.)
      :else (t/vector (t/x point) 0. (t/z point)))))

; ## Creation

(defrecord Cylinder [minimum maximum closed?]
  sh/Shape
  (local-bounds [cyl]
    (local-bounds cyl))
  (prepare-bounds [shape]
    shape)
  (prepare-transform [shape _ _]
    shape)
  (includes? [_ _]
    false)
  (local-intersect [cyl ray object]
    (local-intersect cyl ray object))
  (local-normal [cyl point _]
    (local-normal cyl point)))

(defn cylinder
  ([minimum maximum closed?]
   (->Cylinder minimum maximum closed?))
  ([]
   (cylinder (- (double t/infinity)) t/infinity false)))
