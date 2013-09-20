(ns buggers.coordinates)

(def scale-x 100)
(def scale-y 80)
(def scale-z -40)

(defn to-screen-space
  "Scales a position in game-space to screen-space"
  [[x y z]]
  [(* scale-x x)
   (+ (* scale-y y)
      (* scale-z z))])

(defn to-game-space-z0
  "Translates a position in screen space to the gamespace position if z=0.
   This is a start for selecting things but won't work for elevation"
  [[x y]]
  [(/ x scale-x)
   (/ y scale-y)
   0])
