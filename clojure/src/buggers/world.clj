(ns buggers.world
  (:require [buggers.coordinates :as c]
   [clojure.math.numeric-tower :refer :all])
  (:import  (com.badlogic.gdx Gdx)))

;; Helpers
(defn- has-comps
  [entity & comps]
  (every? true? (map (partial contains? entity) comps)))

(defn- resolve-entity
  [world entity]
  (cond
   (map? entity) entity
   (keyword? entity) (get-in world [:entities entity])
   :else (throw "Error: entity must be the entities component map or id")))

(defn- delta-time
  []
  (.getDeltaTime Gdx/graphics))

(defn- magnitude
  ([[x y z]] (magnitude x y z))
  ([x y z]
     (sqrt (+ (* x x)
              (* y y)
              (* z z)))))

(defn- normalize
  "Normalizes a vector to have a magnitude of 1"
  ([[x y z]] (normalize x y z))
  ([x y z]
     (let [m (magnitude x y z)]
       [(/ x m) (/ y m) (/ z m)])))

;; Actions deals with all the various things you can do in the game.
;; These are meant to be the kinds of actions an AI or player click
;; action would call. This also includes some easy ways to query the
;; gameworld and get various information.

;; This all sort of relies on an implicit schema on the gamestate that
;; I should probably make explicit.

;; Queries
;; =======
;; Queries take the world and some parameters and return various parts
;; ofit.

(defn get-player
  "Gets the player entity."
  [world]
  (get-in world [:entities :player]))

(defn get-with-components
  "Gets a sequence of [id comps] of all the entities that have all of the
   components."
  [world & comps]
  (filter
   (fn [[id cs]] (every? true? (map #(contains? cs %) comps)))
   (:entities world)))

(defn get-mouse-position-z0
  "Gets the z=0 position associated with the mouse's current position."
  [world]
  (let [screen-x (.getX Gdx/input)
        screen-y (.getY Gdx/input)
        screen-width (.getWidth Gdx/graphics)
        screen-height (.getHeight Gdx/graphics)
        off-x (- screen-x (/ screen-width 2))
        off-y (- screen-y (/ screen-height 2))
        [px py pz] (:position (get-player world))
        [ox oy oz] (c/to-game-space-z0 [off-x off-y])]
    [(+ px ox)
     (- py oy)
     (+ pz oz)]))


;; Changes
;; =======
;; Changes take a gameworld and parameters and return a new gameworld.
;; They are all pure functions.
;; They also must be called from within the main render loop. Systems
;; are fair game but if you want to call them from a background job
;; precompute all you need and send the actual updating function
;; (which includes these function calls) in the send execute thing
;; libgdx has built in. TODO: Wrap that functionality in clojure.

;; Generic Changes
(defn set-component
  "Sets a component on the entity with entity-id"
  [world entity comp val]
  (let [e (resolve-entity world entity)]
    (assoc-in world [:entities (:id entity) comp] val)))

(defn create-entity
  "Creates a new entity with an id of id and component map of comps"
  [world id comps]
  (assoc-in world [:entities id] (assoc comps :id id)))

(defn remove-entity
  "Removes an entity from the world"
  [world entity]
  (let [e (resolve-entity world entity)]
    (assoc world :entities (dissoc (:entities world) (:id e)))))

;; Entitiy related.
(defn move-in-direction
  "Moves an entity in a direction at that entities speed.
   Direction should be a vector relative to the entities position."
  [world entity direction]
  (let [e (resolve-entity world entity)
        d (normalize direction)
        {:keys [position speed]} e
        motion (map (partial * (delta-time) speed) d)]
    (set-component world e :position (map + position motion))))
