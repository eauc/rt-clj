; # Cameras

(ns rt-clj.cameras
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:import java.lang.Math)
  (:require [clojure.core.reducers :as cr]
            [clj-progress.core :as pg]
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

(defn camera
  ([^long hs ^long vs ^double fov transform]
   (let [half-view (Math/tan (/ fov 2.))
         aspect (double (/ hs vs))
         half-width (if (>= aspect 1.) half-view (* half-view aspect))
         half-height (if (>= aspect 1.) (/ half-view aspect) half-view)
         pixel-size (/ (* half-width 2) hs)]
     {:hsize hs
      :vsize vs
      :fov fov
      :transform transform
      :inverse-t (m/inverse transform)
      :half-width half-width
      :half-height half-height
      :pixel-size pixel-size}))
  ([hs vs fov]
   (camera hs vs fov (m/id 4))))

; ## Rays

; We can construct rays from the camera "eye" to any pixel in the field of view.
; - first we compute the coordinates of the pixel in camera coordinates.
; - then we transform the pixel and the origin into world coordinates.
; - the ray's origin is the world origin.
; - the ray's direction is the vector from the world-origin to the world-pixel.

(defn pixel-ray [{:keys [^double half-width ^double half-height ^double pixel-size inverse-t]} ^double px ^double py]
  (let [cam-x (- half-width (* (+ px 0.5) pixel-size))
        cam-y (- half-height (* (+ py 0.5) pixel-size))
        world-pixel (m/mul-t inverse-t (t/point cam-x cam-y -1.))
        world-origin (m/mul-t inverse-t t/origin)
        direction (t/norm (t/sub world-pixel world-origin))]
    (r/ray world-origin direction)))

; ## World

; We can render a world as seen from a camera.

(def default-depth 4)

(defn render
  ([{:keys [^long hsize ^long vsize] :as cam}
    world
    {:keys [parallel?] :or {parallel? true}}]
   (let [world (w/prepare world)]
     (pg/init "Rendering" vsize)
     (let [image (if parallel?
                   (cr/fold
                    (int (/ vsize 8))
                    (fn combinef
                      ([] [])
                      ([a b] (concat a b)))
                    (fn reducef
                      ([] [])
                      ([cs y]
                       (pg/tick)
                       (conj cs (mapv #(w/color world (pixel-ray cam % y) default-depth)
                                      (vec (range hsize))))))
                    (vec (range vsize)))
                   (mapv (fn [y]
                           (pg/tick)
                           (mapv #(w/color world (pixel-ray cam % y) default-depth)
                                 (range hsize)))
                         (range vsize)))]
       (pg/done)
       image)))
  ([cam world]
   (render cam world {})))
