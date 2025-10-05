; # Materials

(ns rt-clj.materials
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [rt-clj.colors :as c]
            [rt-clj.pattern-protocol :as pt]
            [rt-clj.tuples :as t]))

; ## Creation

; Materials can be given patterns which replace color.

(def default-material
  {:color c/white
   :ambient 0.1
   :diffuse 0.9
   :reflective 0.
   :refractive-index 1.
   :specular 0.9
   :shadow? true
   :shininess 200
   :transparency 0.})

(def glass
  (assoc default-material
         :transparency 1.
         :refractive-index 1.5))

; ## Lighting

; The Phong reflection model:
; - Ambient reflection is background lighting.
; - Diffuse reflection is light reflected from a matte surface.
; - Specular reflection is the reflection of the light source itself.

; Lighting take into account shadow thanks to an extra parameter `in-shadow`.

; When a point is in the shadow of a light source, only the ambient component is used for lighting.

(defn lighting
  ([material object light position eyev normalv in-shadow?]
   (let [{:keys [color pattern ambient ^double diffuse shininess ^double specular]} material
         color (if pattern (pt/pattern-at-shape pattern object position) color)
         effective-color (c/dot color (:intensity light))
         ambient (c/mul effective-color ambient)]
     (if in-shadow?
       ambient
       (let [lightv (t/norm (t/sub (:position light) position))
             light-dot-normal (t/dot lightv normalv)
             inside? (> 0 light-dot-normal)]
         (if inside?
           ambient
           (let [diffuse (c/mul effective-color (* diffuse light-dot-normal))
                 reflectv (t/reflect (t/sub t/zerov lightv) normalv)
                 reflect-dot-eyev' (t/dot reflectv eyev)
                 reflect-dot-eyev (Math/pow reflect-dot-eyev' shininess)
                 amb-dif (c/add ambient diffuse)
                 specular? (< 0 reflect-dot-eyev)]
             (if-not specular?
               amb-dif
               (let [specular (c/mul (:intensity light) (* reflect-dot-eyev specular))]
                 (c/add amb-dif specular)))))))))
  ([material object light position eyev normalv]
   (lighting material object light position eyev normalv nil)))
