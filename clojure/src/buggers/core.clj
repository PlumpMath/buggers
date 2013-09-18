(ns buggers.core
  (:gen-class)
  (:import (com.badlogic.gdx ApplicationListener Gdx)
           (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute)
           (com.badlogic.gdx.backends.lwjgl LwjglApplication)))

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
    :rock1 {:position [6 6 0]}
    :terrain {[0 0] :grass
              [0 1] :woods
              [0 2] :water}}})

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

(defprotocol LibGDXScene
  (initialize [this] "Initializes Scene")
  (pause [this] "Pauses Scene")
  (resize [this w h] "Resizes window")
  (render [this] "Draw current scene")
  (dispose [this] "not sure yet..."))

(deftype BuggersMainScene [gamestate draw-mesh]
  LibGDXScene
  (initialize [_]
    (reset! gamestate test-scene)
    ;; Set up some triangles to draw.
    (dosync (ref-set draw-mesh (setup-test-triangle))))
  (pause [_] nil)
  (resize [_ w h] nil)
  (render [_]
    (doto (Gdx/gl)
      (.glClear GL10/GL_COLOR_BUFFER_BIT))
    (doto @draw-mesh
      (.render GL10/GL_TRIANGLES 0 3)))
  (dispose [_] nil))

(defn app-listener
  "Creates an ApplicationListner instance that represents the game."
  [scene]
  (let [scene (BuggersMainScene. (atom {}) (ref nil))]
    {:listener (proxy [ApplicationListener] []
                 (resize [w h] (resize scene w h))
                 (create [] (initialize scene))
                 (render [] (render scene))
                 (pause [] (pause scene))
                 (dispose [] (dispose scene)))
     :name "Buggers"
     :width 480
     :height 320}))

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
    
