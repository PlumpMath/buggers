(ns buggers.render
  (:require
   [buggers.world :as w]
   [buggers.coordinates :as c]
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
    (->> (clojure.java.io/resource "planetcute/planetcutecorrect.json")
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
                          (clojure.java.io/resource "planetcute/planetcutecorrect.png")))
        texture (Texture. planetcute-file)]
    (into {}
          (map
           (fn [[k v]] (vector k {:region (apply texture-region texture v)
                                 :w (nth v 2)
                                 :h (nth v 3)}))
           (get-texture-locations)))))

(defn draw-position
  "Calculates where a tile is drawn based on world scale, center screen
   and the width and height of the texture."
  [screen-width screen-height center pos w h]
  (let [[cx cy] (c/to-screen-space center)
        [px py] (c/to-screen-space pos)
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
        (when-let [terrain-type (get-in world [:terrain [x y]])]
          (let [{:keys [region w h]} (terrain-type textures)
                [x y] (draw-position screen-width screen-height center [x y 0] w h)]
            (.draw sprite-batch region (float x) (float y)))))

      ;; Draw Entities with Icons
      (let [icon-ents (w/get-with-components world :icon :position)]
        (doseq [[id comps] icon-ents]
          (let [[x y z] (:position comps)
                {:keys [region w h]} ((:icon comps) textures)
                [dx dy] (draw-position screen-width screen-height center [x y z] w h)]
            (.draw sprite-batch
                   region (float dx) (float (+ dy 40))))))   ;+ 40 for sprites

      (.end sprite-batch)
      )))

(defn create-renderer [] (Renderer. (SpriteBatch.) (planetcute-textures)))
