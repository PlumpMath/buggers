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

;; Takes a number from a range and scales it to [0.0 1.0]
;; Useful for taking perlin noise and scaling it to image values.
(define (clamp min max n)
  (/ (- n min) (- max min)))

;; Scales a real number in [0 1] to an integer in [0-255]
(define (scale-to-rgb n)
  (inexact->exact
   (floor (* 256 n))))

;; Returns a nested list of simplex values.
(define (2d-noise width height #:resolution [resolution 1] #:zoom [zoom 1.0])
  (for/list ([i height])
    (for/list ([j width])
      (simplex (/ i zoom)
               (/ j zoom)))))

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
  (cond [(equal? t 'water) "blue"]
        [(equal? t 'sand) "tan"]
        [(equal? t 'grass) "green"]
        [(equal? t 'forest) "darkgreen"]))

(define (greyscale-color t)
  (color t t t 255))

;; Useful for playing around.
(define (visualize-noise 2d-noise-list #:type [type 'greyscale])
  (define height (length 2d-noise-list))
  (define width (length (first 2d-noise-list)))
  (define to-color
    (if (equal? type 'greyscale)
        (compose
         greyscale-color
         scale-to-rgb
         (curry clamp -1.0 1.0))
        (compose
         terrain-color
         terrain-value)))
  (define color-list
    (map to-color (flatten 2d-noise-list)))
  (color-list->bitmap color-list width height))

;;(scale 10 (visualize-noise (2d-noise 50 50 #:zoom 50) #:type "terrain"))

;;;; GAMEPLAY STUFF

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 600)
(define PLAYER-SPEED 7)

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

;; System: Draw
;; Comps: icon, position
(define (compute-screen-position center pos)
  (let ([x-offset (- (first center) (/ SCREEN-WIDTH 2))]
        [y-offset (- (second center) (/ SCREEN-HEIGHT 2))])
    (list (- (first pos) x-offset)
          (- (second pos) y-offset))))

;; For starters not going to have the terrain be entitites at all.
(define (noise-fn x y #:zoom [zoom 1000])
  (simplex (/ x zoom) (/ y zoom)))

(define (draw-terrain center-screen)
  (define center-x (first center-screen))
  (define center-y (second center-screen))
  (define top-left-x (- center-x (/ SCREEN-WIDTH 2)))
  (define top-left-y (- center-y (/ SCREEN-HEIGHT 2)))
  (for*/fold ([screen (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)])
             ([x (+ 1 (round (/ SCREEN-WIDTH 10)))]
              [y (+ 1 (round (/ SCREEN-HEIGHT 10)))])
    (define terrain-x (- (* 10 x) (- center-x (/ SCREEN-WIDTH 2))))
    (define terrain-y (- (* 10 y) (- center-y (/ SCREEN-HEIGHT 2))))
    (define box-color (terrain-color (terrain-value (noise-fn terrain-x terrain-y))))
    (place-image (square 10 100 box-color) (* 10 x) (* 10 y) screen)))

(define (draw-icons ents)
  (let* ([player (get-player ents)]
         [player-position (position-val (get-component player position))]
         [ents-with-icons (get-with-components ents (list icon))])
    (foldl (λ (e screen)
             (let* ([img (icon-val (get-component e icon))]
                    [position (position-val (get-component e position))]
                    [draw-at (compute-screen-position player-position position)]
                    [x (first draw-at)]
                    [y (second draw-at)])
               (place-image img x y screen)))
           (draw-terrain player-position)
           ents-with-icons)))

(define (render-game state)
  (draw-icons (gamestate-entities state)))

;; Update
(define (get-player-velocity keys-down)
  (let ([apply-directional-velociy
         (λ (k v)
           (cond
             [(key=? k "w") (map + v '(0 -1))]
             [(key=? k "a") (map + v '(-1 0))]
             [(key=? k "s") (map + v '(0 1))]
             [(key=? k "d") (map + v '(1 0))]
             [else v]))])
    (map (curry * PLAYER-SPEED)
         (foldl apply-directional-velociy '(0 0) keys-down))))

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

(define (the-floor-is-made-of-lava)
  (rectangle 1000 1000 100 "red"))

(define test-scene
  (list (entity "ground"
                (list (position '(0 0))
                      (icon (the-floor-is-made-of-lava))))
        (entity "player"
                (list (player)
                      (position '(0 0))
                      (velocity '(0 0))
                      (icon (circle 20 "solid" "blue"))))
        (entity "tree1"
                (list (position '(900 502))
                      (icon (make-shitty-tree-icon))))
        (entity "tree2"
                (list (position '(807 40))
                      (icon (make-shitty-tree-icon))))
        (entity "tree3"
                (list (position '(225 120))
                      (icon (make-shitty-tree-icon))))
        ))

(define (start-scene)
  (big-bang (gamestate test-scene
                       '())
            (on-tick update-game)
            (on-key keydown)
            (on-release keyup)
            (to-draw render-game)))

;;(start-scene)