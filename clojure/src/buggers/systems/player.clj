(ns buggers.systems.player
  (:require [buggers.systems :refer [GameSystem]]
            [buggers.world :as w]
            [buggers.coordinates :as c]
            [clojure.math.numeric-tower :as m])
  (:import  (com.badlogic.gdx Gdx)
            (com.badlogic.gdx Input$Keys)))

;; Player Movement
(defn player-movement-system
  "Moves the player based on pressing the wasd keys"
  []
  (reify GameSystem
    (run [_ world]
      (let [forward (.isKeyPressed Gdx/input Input$Keys/W)
            backward (.isKeyPressed Gdx/input Input$Keys/S)
            left (.isKeyPressed Gdx/input Input$Keys/A)
            right (.isKeyPressed Gdx/input Input$Keys/D)
            player-direction (cond->> [0 0 0]
                                      forward (map + [0 1 0])
                                      backward (map + [0 -1 0])
                                      left (map + [-1 0 0])
                                      right (map + [1 0 0]))]
        (if-not (= [0 0 0] player-direction)
          (w/move-in-direction world (w/get-player world) player-direction)
          world)))))

(defn cursor-tile-system
  "Creates a hilight over the tile the mouse is over.
   Assumes center screen is the player's location. TODO: if camera's get involved things could get tricky."
  []
  (reify GameSystem
    (run [_ world]
      (let [mouse-pos (w/get-mouse-position-z0 world)
            mouse-loc-guy (w/get-entity world :mouse-loc)
            pos (map m/round mouse-pos)]
        (if mouse-loc-guy
          (w/set-component world :mouse-loc :position pos)
          (w/create-entity world :mouse-loc {:position pos
                                             :icon :selector}))))))

(defn health-decay-system
  "Health Decays"
  []
  (reify GameSystem
    (run [_ world]
      (let [hungry-folk (w/get-with-components world :hunger)]
        (reduce
         (fn [w [id comps]] (w/decay-hunger w id))
         world
         hungry-folk)))))

;; This just needs a "closest-to" fn in world
;; It also probably needs a better way to determine what is clicked.
;; This will mean opengl shits which is sort of a bummer.
(defn click-to-eat-system
  "Eats food if you click it and are less than 1 tile away."
  []
  (reify GameSystem
    (run [_ world]
      (let [mouse-pos (w/get-mouse-position-z0 world)
            click-target (second (first (filter
                                         (fn [[id comps]] (contains? comps :food))
                                         (w/get-in-same-tile world mouse-pos)))) ;; Not really good...
            target-position (:position click-target)
            player (w/get-player world)
            player-pos (:position player)]
        (if (and click-target
                 (.justTouched Gdx/input)
                 (< (w/magnitude (map - target-position player-pos)) 1))
          (w/eat world player click-target)
          world)))))
