; # OBJ Files

(ns rt-clj.obj-files
  {:nextjournal.clerk/visibility {:result :hide}
   :nextjournal.clerk/toc true}
  (:require [clojure.string]
            [rt-clj.objects :as os]
            [rt-clj.tuples :as t]))

; The Wavefront OBJ file format is a common format for storing and sharing 3D graphics data. The OBJ format is plain text, which means you can view, edit, and even create these files in any text editor, though it’s much easier to model something in a 3D modeling tool and then export it to OBJ.
;
; [[file:../samples/obj_teapot_low_example.png]]
;
; [[file:../samples/obj_teapot_example.png]]
;
; The OBJ format consists of statements, each of which occupies a single line. Each statement is prefaced with a command, followed by a space-delimited list of arguments. For example, the following OBJ file defines three vertices ( v ), and a triangle ( f , for “face”) that references those vertices.

; ```
; v 1.5 2 1.3
; v 1.4 -1.2 0.12
; v -0.1 0 -1.3
; f 1 2 3
; ```

; Each vertex statement starts with a `v` followed by a space character, and then three integer or floating point numbers delimited by spaces.

; The triangles are introduced with the f command (for "face"), followed by three integers referring to the corresponding vertices. Note that these indices are 1-based, and not 0-based!

; Our ray tracer only knows how to render triangles, though, so it needs to be able to break that polygon apart into triangles.

(defn polygon->triangles
  [normals vertices raw-indices]
  (let [indices (map (fn [raw]
                       (map (fn [i-string]
                              (if (empty? i-string)
                                nil
                                (Integer/parseInt i-string)))
                            (clojure.string/split raw #"/")))
                     raw-indices)
        [v1 & vs] (map #(nth vertices (dec (first %))) indices)
        [n1 & ns] (map (fn [[_ _ vn]]
                         (if (nil? vn)
                           vn
                           (nth normals (dec vn)))) indices)]
    (loop [[v2 & v-rest] vs
           [n2 & n-rest] ns
           triangles []]
      (if-not (and v2 (first v-rest))
        triangles
        (recur v-rest n-rest
               (conj triangles
                     (if (nil? n1)
                       (os/triangle v1 v2 (first v-rest))
                       (os/smooth-triangle v1 v2 (first v-rest)
                                           n1 n2 (first n-rest)))))))))

(defn parse-lines
  ([lines material]
   (let [[current-group
          groups
          normals
          triangles
          vertices] (loop [[l & rest] lines
                           current-group "default"
                           groups {}
                           normals []
                           triangles []
                           vertices []]
                      (if (nil? l)
                        [current-group groups normals triangles vertices]
                        (cond
                          (re-find #"^v " l)
                          (let [[_ x y z] (clojure.string/split l #"\s+")]
                            (recur rest
                                   current-group
                                   groups
                                   normals
                                   triangles
                                   (conj vertices (t/point (Float/parseFloat x)
                                                           (Float/parseFloat y)
                                                           (Float/parseFloat z)))))
                          (re-find #"^vn " l)
                          (let [[_ x y z] (clojure.string/split l #"\s+")]
                            (recur rest
                                   current-group
                                   groups
                                   (conj normals (t/vector (Float/parseFloat x)
                                                           (Float/parseFloat y)
                                                           (Float/parseFloat z)))
                                   triangles
                                   vertices))
                          (re-find #"^f " l)
                          (let [[_ & ps] (clojure.string/split l #"\s+")]
                            (recur rest
                                   current-group
                                   groups
                                   normals
                                   (into
                                    []
                                    (concat triangles
                                            (polygon->triangles normals vertices ps)))
                                   vertices))
                          (re-find #"^g " l)
                          (let [[_ new-group] (clojure.string/split l #"\s+")]
                            (recur rest
                                   new-group
                                   (assoc groups current-group triangles)
                                   normals
                                   []
                                   vertices))
                          :else
                          (recur rest current-group groups normals triangles vertices))))
         groups (assoc groups current-group triangles)]
     {:group (let [named-groups (map (fn [[name children]]
                                       (assoc (os/group children) :name name))
                                     (dissoc groups "default"))]
               (-> (os/group (concat (get groups "default") named-groups))
                   (os/with-material material)))
      :normals normals
      :vertices vertices}))
  ([lines]
   (parse-lines lines nil)))
