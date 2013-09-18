(ns buggers.core
  (:require [clojure.data.json :as json])
  (:gen-class)
  (:import (com.badlogic.gdx ApplicationListener Gdx)
           (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute
                                      Texture)
           (com.badlogic.gdx.graphics.g2d SpriteBatch TextureRegion)
           (com.badlogic.gdx.backends.lwjgl LwjglApplication)))

;; Not sure where this goes yet.

(def test-scene
  {:entities
   {:player {:health 100
             :position [0 0 0]
             :velocity [0 0 0]
             :player nil
             :bugger nil}
    :bugger1 {:health 50
              :position [5 5 0]
              :velocity [0 0 0]
              :bugger nil}
    :rock1 {:position [6 6 0]}}
   :terrain {[0 0] :grass-block
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

;; Creates all the planetcute textures
(defn planetcute-textures []
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
  "Calculates where a tile is drawn based on world scale and center screen"
  [screen-width screen-height center pos]
  (let [[cx cy] (to-screen-space center)
        [px py] (to-screen-space pos)
        x-offset (- cx (/ screen-width 2))
        y-offset (- cy (/ screen-height 2))]
    [(- px x-offset) (- py y-offset)]))

(deftype BuggersMainScene [gamestate draw-mesh sprite-batch]
  LibGDXScene
  (initialize [_]
    (reset! gamestate test-scene)
    ;; Set up graphics.
    (dosync (ref-set sprite-batch (SpriteBatch.)))
    )
  (pause [_] nil)
  (resize [_ w h] nil)
  (render [_]
    (let [textures (planetcute-textures)
          world @gamestate
          center (get-in world [:entities :player :position])]
      ;; Clear Screen
      (doto (Gdx/gl)
        (.glClear GL10/GL_COLOR_BUFFER_BIT))

      ;; Draw Terrain
      (.begin @sprite-batch)
      (doseq [x (range 0 4)
              y (range 0 4)]
        (let [terrain-type (get-in world [:terrain [x y]])
              [x y] (draw-position 1366 768 center [x y 0])]
          (when terrain-type
            (.draw @sprite-batch (terrain-type textures) (float x) (float y)))))
      (.end @sprite-batch)
      ))
   
  (dispose [_] nil))

;; MAIN (or core?)
;; ===============

(defn app-listener
  "Creates an ApplicationListner instance that represents the game."
  [scene]
  (let [scene (BuggersMainScene. (atom {}) (ref nil) (ref nil))]
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
