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
      (let [mouse-pos (w/get-mouse-position-z0 world)]
        (-> world
            (w/remove-entity :mouse-loc)
            (w/create-entity :mouse-loc {:position (map m/round mouse-pos)
                                         :icon :selector}))))))
