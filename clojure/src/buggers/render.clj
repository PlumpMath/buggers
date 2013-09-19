(ns buggers.render
  (:require
   [buggers.systems :as sys]
   [clojure.math.numeric-tower :as math]
   [clojure.data.json :as json])
  (:import
   (com.badlogic.gdx Gdx)
   (com.badlogic.gdx.files FileHandle)
   (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute
                              Texture)
   (com.badlogic.gdx.graphics.g2d SpriteBatch TextureRegion)))

;; Load textures.
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

;; Computing Draw Position.
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

(defprotocol IRenderer
  (render [this world] "Renders the World"))

(defrecord Renderer [sprite-batch textures]
  IRenderer
  (render [_ world]
    (let [screen-width (.getWidth Gdx/graphics)
          screen-height (.getHeight Gdx/graphics)
          center (get-in world [:entities :player :position])
          [cx cy _] center
          range-x (map int (range
                            (math/floor (- cx (/ (/ screen-width 100) 1.5)))
                            (math/ceil (+ cx (/ (/ screen-width 100) 1.5)))))
          range-y (map int (range
                            (math/floor (- cy (/ (/ screen-height 80) 1.5)))
                            (math/ceil (+ cy (/ (/ screen-height 80) 1.5)))))]

      ;; Clear Screen
      (doto (Gdx/gl)
        (.glClear GL10/GL_COLOR_BUFFER_BIT))

      ;; Draw
      (.begin sprite-batch)
      
      ;; Draw Visable Terrain 
      (doseq [y (reverse range-y)
              x range-x]
        (let [terrain-type (get-in world [:terrain [x y]])
              [x y] (draw-position screen-width screen-height center [x y 0] 100 120)]
          (when terrain-type
            (.draw sprite-batch (terrain-type textures) (float x) (float y)))))

      ;; Draw Entities with Icons
      (let [icon-ents (sys/get-with-component world :icon)]
        (doseq [[id comps] icon-ents]
          (let [[x y z] (:position comps)
                [dx dy] (draw-position screen-width screen-height center [x y z] 77 91);; Assuming size of icon, prlly bad.
                texture ((:icon comps) textures)]
            (.draw sprite-batch
                   texture (float dx) (float dy)))))

      (.end sprite-batch)
      )))

(defn create-renderer [] (Renderer. (SpriteBatch.) (planetcute-textures)))
