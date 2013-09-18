(ns buggers.core
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.data.json :as json])
  (:gen-class)
  (:import (com.badlogic.gdx ApplicationListener Gdx)
           (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx Input$Keys)
           (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute
                                      Texture)
           (com.badlogic.gdx.graphics.g2d SpriteBatch TextureRegion)
           (com.badlogic.gdx.backends.lwjgl LwjglApplication)))

;; Not sure where this goes yet.

(def test-scene
  {:entities
   {:player {:health 100
             :position [0.0 0.0 0.0]
             :velocity [0 0 0]
             :player nil
             :bugger nil}
    :bugger1 {:health 50
              :position [5 5 0]
              :velocity [0 0 0]
              :bugger nil}
    :rock1 {:position [6 6 0]}}
   :terrain {[0 -1] :stone-block
             [0 0] :grass-block
             [0 1] :stone-block
             [0 2] :water-block
             [0 3] :water-block
             [1 0] :grass-block
             [1 1] :grass-block
             [1 2] :water-block
             [1 3] :grass-block
             [2 0] :grass-block
             [2 1] :grass-block
             [2 2] :water-block
             [2 3] :grass-block
             [3 0] :grass-block
             [3 1] :grass-block
             [3 2] :water-block
             [3 3] :grass-block}})

;; Drawing
;; =======


;; Textures
;; ========
(defn get-texture-locations []
  (let [get-frames #(get % "frames")]
    (->> (clojure.java.io/resource "planetcute/planetcute.json")
         slurp
         json/read-str
         get-frames
         (map #(vector (get % "filename") (get % "frame")))
         (map (fn [[k v]]
                (vector (-> k
                            clojure.string/lower-case
                            (clojure.string/replace ".png" "")
                            (clojure.string/replace " " "-")
                            keyword)
                        [(get v "x")
                         (get v "y")
                         (get v "w")
                         (get v "h")])))
         (into {})
         )))

(defn texture-region [texture x y w h]
  (TextureRegion. texture x y w h))

(defn planetcute-textures
  "Loads the planetcute texture and computes the texture regions."
  []
  (let [planetcute-file (FileHandle.
                         (clojure.java.io/file
                          (clojure.java.io/resource "planetcute/planetcute.png")))
        texture (Texture. planetcute-file)]
    (into {}
          (map
           (fn [[k v]] (vector k (apply texture-region texture v)))
           (get-texture-locations)))))

(defn draw-test-triangle []
  (let [vertices (float-array [-0.5 -0.5 0 0.5 -0.5 0 0 0.5 0])
        triangles (into-array Short/TYPE [0 1 2])
        attrs (into-array VertexAttribute
         [(VertexAttribute.
           com.badlogic.gdx.graphics.VertexAttributes$Usage/Position
           3 "a_position")])
        mesh (ref nil)]
    (doto (Gdx/gl)
      (.glClear GL10/GL_COLOR_BUFFER_BIT))
    (doto @mesh
      (.render GL10/GL_TRIANGLES 0 3))))

(defn setup-test-triangle []
  (let [vertices (float-array [-0.5 -0.5 0 0.5 -0.5 0 0 0.5 0])
        triangles (into-array Short/TYPE [0 1 2])
        attrs (into-array VertexAttribute
                          [(VertexAttribute.
                            com.badlogic.gdx.graphics.VertexAttributes$Usage/Position
                            3 "a_position")])]
    (doto (Mesh. true 3 3 attrs)
      (.setVertices vertices)
      (.setIndices triangles))))

;; SCENE
;; =====

(defprotocol LibGDXScene
  (initialize [this] "Initializes Scene")
  (pause [this] "Pauses Scene")
  (resize [this w h] "Resizes window")
  (render [this] "Draw current scene")
  (dispose [this] "not sure yet..."))

(defn to-screen-space
  "Scales a position in game-space to screen-space"
  [pos]
  (let [scale-x 100
        scale-y 80
        scale-z -40
        [x y z] pos]
    [(* scale-x x)
     (+ (* scale-y y)
        (* scale-z z))]))

(defn draw-position
  "Calculates where a tile is drawn based on world scale, center screen
   and the width and height of the texture."
  [screen-width screen-height center pos w h]
  (let [[cx cy] (to-screen-space center)
        [px py] (to-screen-space pos)
        x-offset (- cx (/ screen-width 2))
        y-offset (- cy (/ screen-height 2))]
    [(- (- px x-offset) (/ w 2))
     (- (- py y-offset) (/ h 2))]))


;; Helpers for input and vectors, need to put these somewhere.
(defn addv [v1 v2]
  (map + v1 v2))


(deftype BuggersMainScene [gamestate draw-mesh
                           textures sprite-batch]
  LibGDXScene
  (initialize [_]
    (reset! gamestate test-scene)
    ;; Set up graphics.
    (dosync (ref-set sprite-batch (SpriteBatch.)))
    (dosync (ref-set textures (planetcute-textures)))
    )
  (pause [_] nil)
  (resize [_ w h] nil)
  (render [_]
    ;; Input Handling (not sure if it has to be in here yet)
    (let [forward (.isKeyPressed Gdx/input Input$Keys/W)
          backward (.isKeyPressed Gdx/input Input$Keys/S)
          left (.isKeyPressed Gdx/input Input$Keys/A)
          right (.isKeyPressed Gdx/input Input$Keys/D)
          player-speed 0.5
          player-direction (cond->> [0 0 0]
                                    forward (map + [0 1 0])
                                    backward (map + [0 -1 0])
                                    left (map + [-1 0 0])
                                    right (map + [1 0 0]))
          player-motion (map (partial * player-speed) player-direction)]
      (swap! gamestate
             (fn [w]
               (let [pos (get-in w [:entities :player :position])]
                 (println pos)
                 (assoc-in w [:entities :player :position]
                           (map + pos player-motion))))))

    ;; Drawing
    (let [screen-width (.getWidth Gdx/graphics)
          screen-height (.getHeight Gdx/graphics)
          world @gamestate
          center (get-in world [:entities :player :position])
          [cx cy _] center
          range-x (map int (range
                            (math/floor (- cx (/ (/ screen-width 100) 2)))
                            (math/ceil (+ cx (/ (/ screen-width 100) 2)))))
          range-y (map int (range
                            (math/floor (- cy (/ (/ screen-height 80) 2)))
                            (math/ceil (+ cy (/ (/ screen-height 80) 2)))))]

      ;; Clear Screen
      (doto (Gdx/gl)
        (.glClear GL10/GL_COLOR_BUFFER_BIT))

      ;; Draw
      (.begin @sprite-batch)
      ;; Draw Visable Terrain
      
      (doseq [y (reverse range-y)
              x range-x]
        (let [terrain-type (get-in world [:terrain [x y]])
              [x y] (draw-position screen-width screen-height center [x y 0] 100 120)]
          (when terrain-type
            (println x y)
            (.draw @sprite-batch (terrain-type @textures) (float x) (float y)))))

      ;; Draw Player
      (let [[x y] (draw-position screen-width screen-height center [cx cy 0] 77 91)]
        (.draw @sprite-batch
               (:character-horn-girl @textures) (float x) (float y)))
      (.end @sprite-batch)
      ))
   
  (dispose [_] nil))

;; MAIN (or core?)
;; ===============

(defn app-listener
  "Creates an ApplicationListner instance that represents the game."
  [scene]
  (let [scene (BuggersMainScene. (atom {}) (ref nil) (ref nil) (ref nil))]
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
