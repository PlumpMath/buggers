(ns buggers.systems
  "This is the main game logic construct in the game. Just about all
   functionality is expressed as a system that runs on every gametick.")

;; Main protocol
(defprotocol GameSystem
  (run [this world] "Update function that is run in the main render thread
                     inside a swap! on the main gamestate atom. Can access
                     all gdx libraries."))

;; Because the systems must take a world and return a world it is
;; necessary to have many helper functions for altering the world and
;; getting values from the world.

;; Helper Functions for altering the gamestate.

(defn set-component
  "Sets a component on the entity with entity-id"
  [world entity-id comp val]
  (assoc-in world [:entities entity-id comp] val))

(defn get-player
  "Gets the player entity."
  [world]
  (get-in world [:entities :player]))
