(ns rt-clj.light-protocol)

(def light-oversampling
  100)

(defprotocol Light
  (shadow-factor [shape world point light-position] "Returns the light's shadow factor at point"))
