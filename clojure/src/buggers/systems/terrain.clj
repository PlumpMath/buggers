(ns buggers.systems.terrain
  (:require [buggers.systems :refer [GameSystem]]
            [buggers.world :as w]
            [clojure.math.numeric-tower :as math])
  (:import (SimplexNoise)))

;; Distance from the player in gamespace that we render. 11 should
;; cover enough so we see the whole screen.
(def render-distance 11)

;; TODO: figure out how to change a seed.
(defn noise
  "Get the noise value for the location [x y]"
  [x y]
  (SimplexNoise/noise (/ x 15) (/ y 15)))

(defn terrain-type
  "Gets the terrain type for the noise value. For now the keyword returned
   is the same one used to store the texture."
  [n]
  (condp > n
    -0.5 :water-block
    0 :dirt-block
    0.75 :grass-block
    :stone-block))

(defn terrain-system
  []
  (reify GameSystem
    (run [_ world]
      (let [current-terrain (:terrain world)
            [x y _] (:position (w/get-player world))
            range-x (map int (range (- (math/floor x) render-distance)
                                    (+ (math/floor x) render-distance)))
            range-y (map int (range (- (math/floor y) render-distance)
                                    (+ (math/floor y) render-distance)))
            new-terrain (into {} (for [x range-x
                                       y range-y
                                       :when (not (contains? current-terrain [x y]))]
                                   (vector [x y] (terrain-type (noise x y)))))]
        (assoc world :terrain (merge current-terrain new-terrain))))))
