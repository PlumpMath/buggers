(ns buggers.systems
  "This is the main game logic construct in the game. Just about all
   functionality is expressed as a system that runs on every gametick.")

;; Main protocol
(defprotocol GameSystem
  (run [this world] "Update function that is run in the main render thread
                     inside a swap! on the main gamestate atom. Can access
                     all gdx libraries."))
