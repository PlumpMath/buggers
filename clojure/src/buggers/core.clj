(ns buggers.core
  (:require
   [buggers.systems :as sys]
   [buggers.systems.terrain :refer [terrain-system]]
   [buggers.systems.player :refer [player-movement-system cursor-tile-system health-decay-system click-to-eat-system]]
   [buggers.render :as r])
  (:gen-class)
  (:import
   (com.badlogic.gdx ApplicationListener)
   (com.badlogic.gdx.backends.lwjgl LwjglApplication)))

;; Not sure where this goes yet.

(def test-scene
  {:entities
   {:player {:health 100
             :position [0 0 0]
             :icon :character-horn-girl
             :speed 5
             :player nil
             :bugger nil
             :hunger 150
             :id :player}
    :bugger1 {:health 50
              :hunger 150
              :position [5 5 0]
              :bugger nil
              :icon :enemy-bug
              :id :bugger1}
    :food1 {:position [-5 -5 0]
            :icon :tree-short
            :food :yellow
            :id :food1}
    :food2 {:position [0 -5 0]
            :icon :tree-short
            :food :green
            :id :food2}
    :tree1 {:position [0 3 0]
            :icon :tree-tall
            :id :tree1
            :tree nil}
    :tree2 {:position [3 3 0]
            :icon :tree-tall
            :id :tree2
            :tree nil}}})

(def test-systems
  [(terrain-system)
   (player-movement-system)
   (cursor-tile-system)
   (health-decay-system)
   (click-to-eat-system)])

;; SCENE
;; =====

(defprotocol LibGDXScene
  (initialize [this] "Initializes Scene")
  (pause [this] "Pauses Scene")
  (resize [this w h] "Resizes window")
  (render [this] "Draw current scene")
  (dispose [this] "not sure yet..."))

(deftype BuggersMainScene [gamestate
                           systems
                           renderer]
  LibGDXScene
  (initialize [_]
    ;; Test Scene
    (reset! gamestate test-scene)
    (reset! systems test-systems)
    ;; Set up graphics.
    (reset! renderer (r/create-renderer))
    )
  (pause [_] nil)
  (resize [_ w h] nil)
  (render [_]
    ;; Run Systems!
    (doseq [s @systems]
      (swap! gamestate (partial sys/run s)))

    ;; Draw!
    (r/render @renderer @gamestate))
  (dispose [_] nil))

(defn app-listener
  "Creates an ApplicationListner instance that represents the game."
  [scene]
  (let [scene (BuggersMainScene. (atom {})
                                 (atom [])
                                 (atom nil))]
    {:listener (proxy [ApplicationListener] []
                 (resize [w h] (resize scene w h))
                 (create [] (initialize scene))
                 (render [] (render scene))
                 (pause [] (pause scene))
                 (dispose [] (dispose scene)))
     :name "Buggers"
     :width 1366
     :height 768}))

(defn -main
  "Starts the main game"
  [& args]
  (let [scene (or (keyword (first args)) :main)
        listener (app-listener scene)]
    (LwjglApplication. (:listener listener)
                       (:name listener)
                       (:width listener)
                       (:height listener)
                       false ;research gl2
                       )))
