(ns rt-clj.world-protocol)

(defprotocol
  World
  (shadowed? [world point light-position] "Check if point is shadowed light-position"))
