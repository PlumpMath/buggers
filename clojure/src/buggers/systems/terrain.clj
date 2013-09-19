(ns buggers.systems.terrain
  (:require [buggers.systems :refer :all]
            [clojure.math.numeric-tower :as math])
  (:import (SimplexNoise)))

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

(defrecord TerrainSystem [render-distance]
  GameSystem
  (run [_ w]
    (let [current-terrain (:terrain w)
          [x y _] (:position (get-player w))
          range-x (map int (range (- (math/floor x) render-distance)
                                  (+ (math/floor x) render-distance)))
          range-y (map int (range (- (math/floor y) render-distance)
                                  (+ (math/floor y) render-distance)))
          new-terrain (into {} (for [x range-x
                                     y range-y
                                     :when (not (contains? current-terrain [x y]))]
                                 (vector [x y] (terrain-type (noise x y)))))]
      (assoc w :terrain (merge current-terrain new-terrain)))))

(defn terrain-system [] (TerrainSystem. 11))
