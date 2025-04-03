(ns check-opencl-config
  (:require [clojure.pprint :refer [pprint]]
            [uncomplicate.clojurecl.core :as cl]
            [uncomplicate.commons.core :refer [info]]))

(defn -main
  []
  (println "Platforms:")
  (pprint
   (map info (cl/platforms)))
  (println "Devices:")
  (pprint
   (map info (cl/devices (first (cl/platforms))))))
