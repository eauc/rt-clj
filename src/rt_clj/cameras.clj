; # Cameras

(ns rt-clj.cameras
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [clojure.core.reducers :as cr]
            [clj-progress.core :as pg]
            [rt-clj.colors :as c]
            [rt-clj.matrices :as m]
            [rt-clj.rays :as r]
            [rt-clj.tuples :as t]
            [rt-clj.worlds :as w]))

; [[file:../samples/camera_world_example.png]]

; ## Creation

; Cameras are defined by:
; - a width in pixels.
; - a height in pixels.
; - a field of view in radians.

; We can access the size of each pixels, computed from the greatest of hsize & vsize, and the field of view.

(defrecord Camera
           [hsize
            vsize
            fov
            focal-length
            aperture
            default-depth
            parallel-depth
            oversampling
            blur-oversampling
            transform
            inverse-t
            half-width
            half-height
            pixel-size])

(defn camera
  ([{:keys [^long hsize ^long vsize ^double fov focal-length aperture transform default-depth parallel-depth blur-oversampling oversampling] :as options
     :or {transform (m/id 4)
          focal-length 1.
          aperture 0.
          default-depth 4
          parallel-depth 0
          oversampling 1
          blur-oversampling 1}}]
   (let [half-view (* (Math/tan (/ fov 2.)) focal-length)
         aspect (double (/ hsize vsize))
         half-width (if (>= aspect 1.) half-view (* half-view aspect))
         half-height (if (>= aspect 1.) (/ half-view aspect) half-view)
         pixel-size (/ (* half-width 2) hsize)]
     (map->Camera
      (merge
       options
       {:hsize hsize
        :vsize vsize
        :fov fov
        :focal-length focal-length
        :aperture aperture
        :default-depth default-depth
        :parallel-depth parallel-depth
        :oversampling oversampling
        :blur-oversampling blur-oversampling
        :transform transform
        :inverse-t (m/inverse transform)
        :half-width half-width
        :half-height half-height
        :pixel-size pixel-size})))))

; ## Rays

; We can construct rays from the camera "eye" to any pixel in the field of view.
; - first we compute the coordinates of the pixel in camera coordinates.
; - then we transform the pixel and the origin into world coordinates.
; - the ray's origin is the world origin.
; - the ray's direction is the vector from the world-origin to the world-pixel.

(defn ray-for-coordinates
  [{:keys [inverse-t focal-length aperture blur-oversampling]} x y]
  (let [world-pixel (m/mul-t inverse-t (t/point x y (- focal-length)))
        aperture (* focal-length aperture)]
    (for [_ (range blur-oversampling)]
      (let [dv (if (< 1 blur-oversampling)
                 (t/vector (t/rand-dv aperture)
                           (t/rand-dv aperture)
                           0.)
                 (t/vector 0. 0. 0.))
            world-origin (m/mul-t inverse-t (t/add t/origin dv))
            direction (t/norm (t/sub world-pixel world-origin))]
        (r/ray world-origin direction)))))

(defn pixel-rays
  [{:keys [^double half-width ^double half-height ^double pixel-size oversampling] :as cam} ^double px ^double py]
  (let [offset (/ 1. oversampling)
        start-offset (/ offset 2.)
        cam-xys (for [i (range oversampling)
                      j (range oversampling)]
                  (vector
                   (- half-width (* (+ px (* i offset) start-offset) pixel-size))
                   (- half-height (* (+ py (* j offset) start-offset) pixel-size))))]
    (mapcat (fn [[x y]] (ray-for-coordinates cam x y)) cam-xys)))

; ## World

; We can render a world as seen from a camera.

(defn- render-pixel
  [cam world x y]
  (let [{:keys [default-depth]} cam
        rs (pixel-rays cam x y)
        colors (mapv #(w/color world % default-depth) rs)]
    (c/avg colors)))

(defn- render-line
  [cam world y]
  (let [{:keys [hsize]} cam]
    (mapv #(render-pixel cam world % y) (range hsize))))

(defn render
  ([{:keys [^long vsize parallel-depth] :as cam} world]
   (let [world (w/prepare world)
         parallel? (< 0 parallel-depth)]
     (pg/init "Rendering" vsize)
     (let [image (if parallel?
                   (cr/fold
                    (int (/ vsize parallel-depth))
                    (fn combinef
                      ([] [])
                      ([a b] (concat a b)))
                    (fn reducef
                      ([] [])
                      ([cs y]
                       (pg/tick)
                       (conj cs (render-line cam world y))))
                    (vec (range vsize)))
                   (mapv (fn [y]
                           (pg/tick)
                           (render-line cam world y))
                         (range vsize)))]
       (pg/done)
       image))))
