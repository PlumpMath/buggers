#lang racket
#| 
   Working my way through realm of racket for a bit then making a game called
   lil buggers which is going to be real fun.
|#
(require 2htdp/universe
         2htdp/image
         2htdp/planetcute
         "noise.rkt")

;;;; PRECEDURAL GENERATION STUFF

;; Seed, not sure how to use it yet.
(define SEED "someshit")

;; Takes the perlin noise value and returns
(define (terrain-value x)
;;  .75 - 1.0  -> forest
;;  .01 -  .74 -> grass
;; -.75 - 0.0  -> sand
;; -1.0 - -.74 -> water
  (cond [(< x -0.75) 'water]
        [(< x 0) 'sand]
        [(< x .75) 'grass]
        [else 'forest]))

(define (terrain-color t)
  (cond [(equal? t 'water)  "blue"]
        [(equal? t 'sand)   "tan"]
        [(equal? t 'grass)  "green"]
        [(equal? t 'forest) "darkgreen"]))

(define (terrain-tile t)
  (cond [(equal? t 'water)  water-block]
        [(equal? t 'sand)   dirt-block]
        [(equal? t 'grass)  grass-block]
        [(equal? t 'forest) stone-block]))

;;;; GAMEPLAY STUFF

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 600)

;; Hopefully if we use these constants if I get other tiles with a slightly
;; different isomorphic angle everything will still work.
(define TILE-X 100)
(define TILE-Y 80)
(define TILE-Z -40)
(define PLAYER-SPEED 1/10)

;; GameState
(struct gamestate (entities keysdown) #:transparent)

;; Entities
(struct entity (id components) #:transparent)

;; Components
(struct position (val) #:mutable #:transparent)
(struct velocity (val) #:mutable #:transparent)
(struct icon (val) #:mutable #:transparent)
(struct player () #:transparent)

(define (is-component-type t c)
    (let ([checker
           (cond
             [(equal? t position) position?]
             [(equal? t velocity) velocity?]
             [(equal? t icon) icon?]
             [(equal? t player) player?])])
      (checker c)))

(define (get-with-components entities comp-types)
  (define (has-component? entity comp-type)
    (ormap
     (λ (component) (is-component-type comp-type component))
     (entity-components entity)))
  (filter
   (λ (entity)
     (andmap (curry has-component? entity) comp-types))
   entities))

;; Get a component from an entity
(define (get-component ent comp-type)
  (let ([comp-list (filter
                    (curry is-component-type comp-type)
                    (entity-components ent))])
    (if (empty? comp-list)
        #f
        (first comp-list))))

;; Returns the player entity
(define (get-player ents)
  (first (get-with-components ents (list player))))


;; Noise
(define (noise-fn x y #:zoom [zoom 10])
  (simplex (/ x zoom) (/ y zoom)))

;; Draw
(define (compute-screen-position center pos)
  (let ([x-offset (- (first center) (/ SCREEN-WIDTH 2))]
        [y-offset (- (second center) (/ SCREEN-HEIGHT 2))])
    (list (- (first pos) x-offset)
          (- (second pos) y-offset))))

;; TODO: probably want vectors and vector math for this.
(define (game-space->screen-space pos)
  (list (* TILE-X (first pos))
        (+ (* TILE-Y (second pos))
           (* TILE-Z (third pos)))))

;; This transformation isn't 1-1 because it's 2 dimentions to 3. This could end up
;; being an issue when trying to click something that's on various z levels but can
;; deal with that when I get to it. This function currently assumes everything is at
;; z = 0
(define (screen-space->game-space loc)
  (list (/ (first loc) 100)
        (/ (second loc) 80)
        0))

(define (draw-icons ents)
  (let* ([player (get-player ents)]
         [player-position (position-val (get-component player position))]
         [ents-with-icons (get-with-components ents (list icon))])
    (foldl (λ (e screen)
             (let* ([img (icon-val (get-component e icon))]
                    [position (position-val (get-component e position))]
                    
                    [draw-at
                     (compute-screen-position
                      (game-space->screen-space player-position)
                      (game-space->screen-space position))]
                    
                    [x (first draw-at)]
                    [y (second draw-at)])
               
               (place-image img x y screen)))
           ;;(draw-terrain player-position)
           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)
           ents-with-icons)))

(define (render-game state)
  (draw-icons (gamestate-entities state)))

;; Update
(define (get-player-velocity keys-down)
  (let ([apply-directional-velociy
         (λ (k v)
           (cond
             [(key=? k "w") (map + v '(0 -1 0))]
             [(key=? k "a") (map + v '(-1 0 0))]
             [(key=? k "s") (map + v '(0 1 0))]
             [(key=? k "d") (map + v '(1 0 0))]
             [else v]))])
    (map (curry * PLAYER-SPEED)
         (foldl apply-directional-velociy '(0 0 0) keys-down))))

;; Takes the list of entities, replaces the component of comp-type 
;; on the entity with id id with new-component.
;; Returns a new list of entities.
;; This can be used to swap non mutable components but with mutable ones
;; it's pretty overkill.
(define (swap-component ents ent-id comp-type new-comp)
  (let* ([ent (first (filter (λ (e) (equal? (entity-id e) ent-id)) ents))]
         [others (filter (λ (e) (not (equal? (entity-id e) ent-id))) ents)]
         [comps (filter 
                 (λ (c) (not (is-component-type comp-type c)))
                 (entity-components ent))]
         [new-ent (struct-copy entity ent
                               [components (cons new-comp comps)])])
    (cons new-ent others)))

;; System
;; Comps: velocity player
(define (set-player-velocity w)
  (let* ([ks (gamestate-keysdown w)]
         [v (get-player-velocity ks)]
         [ents (gamestate-entities w)]
         [player (get-player ents)]
         [players-current-velocity (get-component player velocity)])
    (set-velocity-val! players-current-velocity v)
    w))

(define (update-position! e)
  (let ([v (get-component e velocity)]
        [p (get-component e position)])
    (and v p (set-position-val! p (map +
                                       (velocity-val v)
                                       (position-val p))))))

;; System
;; Comps: velocity position
(define (apply-velocity-to-position w)
  (let* ([ents (gamestate-entities w)])
    (for ([e ents])
      (update-position! e))
    w))

(define (update-game w)
  ((compose apply-velocity-to-position set-player-velocity) w))

;; KeyPresses
(define (keydown w a-key)
  (let ([keys (gamestate-keysdown w)])
    (if (not (member a-key keys))
        (struct-copy gamestate w [keysdown (cons a-key keys)])
        w)))

(define (keyup w a-key)
  (let ([keys (gamestate-keysdown w)])
    (struct-copy gamestate w
                 [keysdown (filter (λ (k) (not (key=? a-key k))) keys)])))

(define (make-shitty-tree-icon)
  (overlay/align/offset
   "middle" "top"
   (above (triangle 50 "solid" "darkgreen")
          (triangle 50 "solid" "darkgreen")
          (triangle 50 "solid" "darkgreen"))
   0 10
   (rectangle 10 150 "solid" "brown")))

(define (get-terrain-tile x y)
  (terrain-tile (terrain-value (noise-fn x y))))

;; Start out just generating some entities for the terrain. Don't
;; worry about making it a system that generates as you walk yet.
(define (generate-some-initial-terrain)
  (for*/list ([y (range -10 10)]
              [x (range -10 10)])
    ;; TODO: real id because id's will matter soon.
    (entity (string-append "terrain:" (number->string x) "," (number->string y) "," "0")
            (list (icon (get-terrain-tile x y))
                  (position (list x y 0))))))

;; Initial scene.
(define test-scene
  (append
   (generate-some-initial-terrain)
   (list
    (entity "rock"
            (list (position '(1 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(2 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(3 1 1))
                  (icon stone-block)))
    (entity "rock"
            (list (position '(2 1 2))
                  (icon stone-block)))
    (entity "player"
            (list (player)
                  (position '(5 5 0))
                  (velocity '(0 0 0))
                  (icon (circle 20 "solid" "blue"))))
    (entity "tree1"
            (list (position '(900 502 0))
                  (icon (make-shitty-tree-icon))))
    (entity "tree2"
            (list (position '(807 40 0))
                  (icon (make-shitty-tree-icon))))
    (entity "tree3"
            (list (position '(225 120 0))
                  (icon (make-shitty-tree-icon))))
    )))

(define (start-scene)
  (big-bang (gamestate test-scene
                       '())
            (on-tick update-game)
            (on-key keydown)
            (on-release keyup)
            (to-draw render-game)))

;;(start-scene)
