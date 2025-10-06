; # Worlds

(ns rt-clj.worlds
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.colors :as c]
            [rt-clj.intersections :as i]
            [rt-clj.lights :as l]
            [rt-clj.materials :as m]
            [rt-clj.object-protocol :as o]
            [rt-clj.objects :as os]
            [rt-clj.rays :as r]
            [rt-clj.transformations :as tr]
            [rt-clj.tuples :as t]
            [rt-clj.world-protocol :as wp]))

(defn with-objects [w objects]
  (assoc w
         :objects objects
         :objects-with-shadow (filter #(get-in % [:material :shadow?] true) objects)))

(defn prepare [{:keys [objects] :as w}]
  (let [objects' (mapv #(o/prepare %) objects)]
    (with-objects w objects')))

; ## Intersections with rays
;
; The `intersect` function on a world should iterate over the list of objects and return all intersections with the ray.
;
; We return the intersections in sorted order since this will help with some future algo.

(defn intersect
  ([w ray shadow?]
   (let [objs (if shadow? (:objects-with-shadow w) (:objects w))
         is (into
             []
             (comp (mapcat #(o/intersect % ray))
                   (filter #(or (not shadow?) (< 0 ^double (:t %)))))
             objs)]
     (vec (sort-by :t is))))
  ([w ray]
   (intersect w ray nil)))

; ## Shadows

; A point is shadowed with regards to a light, if there is any object between the point and the light.

; We can compute this by casting a ray from the point to the light, and see if there is a hit closer than the distance to the light.

(defn shadowed? [w p l-p]
  (let [p->l (t/sub l-p p)
        d (t/mag p->l)
        ray (r/ray p (t/norm p->l))
        hits (intersect w ray :shadow)
        {:keys [^double t]} (i/hit hits)]
    (boolean (and t (> d t)))))

; ## Reflection

; [[file:../samples/reflection_example.png]]

(declare color)

(defn reflected-color
  [world hit comps ^long remaining]
  (let [reflective (get-in hit [:object :material :reflective])]
    (if (or (>= 0 remaining)
            (= 0. reflective))
      c/black
      (let [{:keys [point reflectv]} comps
            reflect-ray (r/ray point reflectv)
            col (color world reflect-ray (dec remaining))]
        (c/mul col reflective)))))

; ## Refraction

; [[file:../samples/refraction_example.png]]

; We must stop refraction
; - if we reached maximum recursive depth
; - if the material is opaque
; - if we face total internal reflection

; To calculate the angle of refraction:
; - we first calculate the refractive indices ratio
; - cos(angle of incidence) is the dot product of the eye and normal vectors
; - we can then calculate sine(refraction angle)^2
; - if this is superior to 1, we have total internal refaction

(defn refracted-color
  [world hit comps ^long remaining]
  (let [transparency (get-in hit [:object :material :transparency])]
    (if (or (= 0 remaining)
            (= 0. transparency))
      [c/black 1.]
      (let [{:keys [normalv eyev under-point n]} comps
            [^double n1 ^double n2] n
            n-ratio ^double (/ n1 n2)
            cos-i (t/dot eyev normalv)
            sin-t-square (* n-ratio n-ratio (- 1 (* cos-i cos-i)))]
        (if (< 1 sin-t-square)
          [c/black 1.]
          (let [cos-t (Math/sqrt (- 1 sin-t-square))
                direction (t/sub (t/mul normalv (- (* n-ratio cos-i) cos-t))
                                 (t/mul eyev n-ratio))
                refract-ray (r/ray under-point direction)
                cos (if (> n1 n2) cos-t cos-i)
                r0 ^double (Math/pow (/ (- n1 n2) (+ n1 n2)) 2.)]
            [(c/mul (color world refract-ray (dec remaining))
                    transparency)
             (+ r0 (* (- 1. r0) (Math/pow (- 1. cos) 5)))]))))))

; ## Shading

; We can calculate the shading of an object in the world, from a prepared hit point.

(defn shade-hit [w hit comps remaining]
  (let [{:keys [object]} hit
        {:keys [material]} object
        {:keys [^double reflective ^double transparency]} material
        {:keys [point eyev normalv]} comps
        lights (mapv
                #(l/shadowed % w point)
                (:lights w))
        surface (m/lighting
                 material object
                 (:ambient w) lights
                 point eyev normalv)
        reflected (reflected-color w hit comps remaining)
        [refracted ^double reflectance] (refracted-color w hit comps remaining)]
    (if (and (> reflective 0.) (> transparency 0.))
      (c/add surface
             (c/add (c/mul reflected reflectance)
                    (c/mul refracted (- 1. reflectance))))
      (c/add surface
             (c/add reflected refracted)))))

; ## Color

; Different cases:
; - when the ray misses all objects, returns black.
; - when the ray hits an object in front of view, calculate shading at intersection point.
; - when the ray hits an object behind the view, ignore this intersection.

(defn color [w ray remaining]
  (let [xs (intersect w ray)
        hit? (i/hit xs)]
    (if (not hit?)
      c/black
      (shade-hit w hit? (i/prepare-hit hit? ray xs) remaining))))

; ## Creation
;
; A world store all the objects and lights of a scene.

(defrecord World 
  [objects objects-with-shadow ambient lights]
  wp/World
  (shadowed? [world point light-position]
    (shadowed? world point light-position)))

(defn world
  ([{:keys [objects ambient lights]
     :or {objects []
          ambient c/white
          lights []}}]
   (-> (map->World
        {:ambient ambient
         :lights lights})
       (with-objects objects)))
  ([]
   (world {})))

; For testing purpose we can create a default world with:
; - 2 spheres centered at origin with different radii.
; - 1 light source at `[-10,10,-10]`

(defn default-world []
  (world {:objects [(-> (os/sphere)
                        (os/with-material
                          {:color (c/color 0.8 1.0 0.6)
                           :diffuse 0.7
                           :specular 0.2}))
                    (-> (os/sphere)
                        (os/with-transform
                          (tr/scaling 0.5 0.5 0.5)))]
          :lights [(l/point-light (t/point -10. 10. -10.) (c/color 1. 1. 1.))]}))
