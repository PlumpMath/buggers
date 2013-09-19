(ns buggers.core
  (:require
   [buggers.systems :as sys]
   [buggers.systems.terrain :refer [terrain-system]]
   [buggers.render :as r])
  (:gen-class)
  (:import
   (com.badlogic.gdx ApplicationListener Gdx)
   (com.badlogic.gdx Input$Keys)
   (com.badlogic.gdx.backends.lwjgl LwjglApplication)))

;; Not sure where this goes yet.

(def test-scene
  {:entities
   {:player {:health 100
             :position [0.0 0.0 0.0]
             :speed 5
             :player nil
             :bugger nil}
    :bugger1 {:health 50
              :position [5 5 0]
              :bugger nil}
    :rock1 {:position [6 6 0]}}})

(def test-systems
  [(terrain-system)])


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

    ;; Input Handling (not sure if it has to be in here yet)
    (let [forward (.isKeyPressed Gdx/input Input$Keys/W)
          backward (.isKeyPressed Gdx/input Input$Keys/S)
          left (.isKeyPressed Gdx/input Input$Keys/A)
          right (.isKeyPressed Gdx/input Input$Keys/D)
          delta (.getDeltaTime Gdx/graphics)
          player-direction (cond->> [0 0 0]
                                    forward (map + [0 1 0])
                                    backward (map + [0 -1 0])
                                    left (map + [-1 0 0])
                                    right (map + [1 0 0]))
          player-motion (map (partial * delta)
                             player-direction)]
      (swap! gamestate
             (fn [w]
               (let [pos (get-in w [:entities :player :position])
                     speed (get-in w [:entities :player :speed])]
                 (assoc-in w [:entities :player :position]
                           (map + pos (map (partial * speed) player-motion)))))))

    ;; Run Systems!
    (doseq [s @systems]
      (swap! gamestate (partial sys/run s)))

    ;; Draw!
    (r/render @renderer @gamestate)
   )
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
