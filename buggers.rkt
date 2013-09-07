#lang racket
#| 
   Working my way through realm of racket for a bit then making a game called
   lil buggers which is going to be real fun.
|#
(require 2htdp/universe 2htdp/image)

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 600)
(define PLAYER-SPEED 10)

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
           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)
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

(define (start-scene)
  (big-bang (gamestate (list (entity 1 (list (player)
                                             (position '(0 0))
                                             (velocity '(0 0))
                                             (icon (circle 20 100 "blue"))))
                             (entity 2 (list (position '(900 502))
                                             (icon (circle 14 100 "red"))))
                             (entity 3 (list (position '(807 40))
                                             (icon (circle 6 100 "green"))))
                             (entity 4 (list (position '(225 120))
                                             (icon (circle 88 100 "black")))))
                       '())
            (on-tick update-game)
            (on-key keydown)
            (on-release keyup)
            (to-draw render-game)))

(start-scene)