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
   :terrain {[0 0] :grass
             [0 1] :woods
             [0 2] :water
             [0 3] :water
             [1 0] :grass
             [1 1] :grass
             [1 2] :water
             [1 3] :grass
             [2 0] :grass
             [2 1] :grass
             [2 2] :water
             [2 3] :grass
             [3 0] :grass
             [3 1] :grass
             [3 2] :water
             [3 3] :grass}})

;; Drawing
;; =======


;; Textures
;; ========
(def texture-locations
  {:stone-block [208 586 101 123],
   :gem-blue [206 155 101 113],
   :shadow-north [513 413 101 91],
   :chest-open [686 2 101 121],
   :roof-south-east [824 288 101 123],
   :roof-south [105 413 101 121],
   :enemy-bug [105 155 99 77],
   :character-boy [105 2 67 89],
   :heart [618 155 89 91],
   :character-princess-girl [403 2 75 99],
   :roof-north-west [618 288 101 85],
   :dirt-block [789 2 101 121],
   :shadow-north-west [460 413 51 51],
   :roof-north-east [515 288 101 85],
   :shadow-side-west [616 413 51 41],
   :key [709 155 59 99],
   :shadow-north-east [437 413 21 51],
   :selector [311 413 101 171],
   :door-tall-open [2 155 101 79],
   :grass-block [515 155 101 131],
   :brown-block [2 2 101 121],
   :ramp-west [208 288 101 119],
   :ramp-north [2 288 101 81],
   :shadow-south [745 413 101 21],
   :shadow-east [414 413 21 81],
   :tree-short [311 586 99 107],
   :roof-west [208 413 101 119],
   :shadow-west [848 413 51 81],
   :ramp-east [873 155 101 119],
   :rock [311 288 99 99],
   :roof-east [412 288 101 119],
   :gem-orange [412 155 101 113],
   :water-block
   [824 586 101 121],
   :window-tall [2 755 101 161],
   :door-tall-closed [892 2 101 151],
   :character-cat-girl [174 2 69 91],
   :wood-block [105 755 101 121],
   :speechbubble [901 413 101 107],
   :tree-ugly [515 586 101 115],
   :character-horn-girl [245 2 77 91],
   :stone-block-tall [105 586 101 163],
   :character-pink-girl [324 2 77 89],
   :chest-lid [583 2 101 85],
   :wall-block-tall [618 586 101 167],
   :gem-green [309 155 101 113],
   :ramp-south [105 288 101 121],
   :star [2 586 101 101],
   :chest-closed [480 2 101 121],
   :roof-north [721 288 101 81],
   :wall-block [721 586 101 127],
   :plain-block [770 155 101 121],
   :shadow-south-east [669 413 21 51],
   :roof-south-west [2 413 101 123],
   :tree-tall [412 586 101 137],
   :shadow-south-west [692 413 51 51]})

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
           texture-locations))))

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
    (let [textures (planetcute-textures)]
      ;; Clear Screen
      (doto (Gdx/gl)
        (.glClear GL10/GL_COLOR_BUFFER_BIT))

      ;; Draw Grass Block
      (doto @sprite-batch
        (.begin)
        (.draw (:stone-block textures) (float 10) (float 10))
        (.end))
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
