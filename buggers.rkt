#lang racket
#| 
   Working my way through realm of racket for a bit then making a game called
   lil buggers which is going to be real fun.

   Might write a little 2d physics engine just to practice.
|#

(require 2htdp/universe 2htdp/image)

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 600)
(define PLAYER-SPEED 10)

;; Buggers!
(struct gamestate (buggers keysdown) #:transparent)
(struct bugger (velocity position icon) #:transparent)

;; Draw
(define (draw-buggers buggers)
  (foldl (lambda (b screen)
           (let* ([img (bugger-icon b)]
                  [position (bugger-position b)]
                  [x (first position)]
                  [y (second position)])
             (place-image img x y screen)))
         (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)
         buggers))

(define (render-game state)
  (draw-buggers (gamestate-buggers state)))

;; Update
;; TODO
(define (get-player-velocity keys-down)
  (let ([apply-directional-velociy
         (lambda (k v)
           (cond
             [(key=? k "w") (map + v '(0 -1))]
             [(key=? k "a") (map + v '(-1 0))]
             [(key=? k "s") (map + v '(0 1))]
             [(key=? k "d") (map + v '(1 0))]
             [else v]))])
    (map (curry * PLAYER-SPEED)
         (foldl apply-directional-velociy '(0 0) keys-down))))
  

(define (apply-player-velocity w)
  (let* ([ks (gamestate-keysdown w)]
         [v (get-player-velocity ks)]
         [bs (gamestate-buggers w)]
         [player (first bs)])
    (struct-copy gamestate w
                 [buggers
                  (cons
                   (struct-copy bugger player [velocity v])
                   (rest (gamestate-buggers w)))])))

(define (update-bugger-position b)
  (let ([v (bugger-velocity b)]
        [p (bugger-position b)])
    (struct-copy bugger b [position (map + v p)])))

(define (update-buggers-positions w)
  (let* ([buggers (gamestate-buggers w)]
         [updated-buggers (map update-bugger-position buggers)])
    (struct-copy gamestate w [buggers updated-buggers])))

(define (update-game w)
  ((compose update-buggers-positions apply-player-velocity) w))

;; KeyPresses
(define (keydown w a-key)
  (let ([keys (gamestate-keysdown w)])
    (if (not (member a-key keys))
        (struct-copy gamestate w [keysdown (cons a-key keys)])
        w)))

(define (keyup w a-key)
  (let ([keys (gamestate-keysdown w)])
    (struct-copy gamestate w
                 [keysdown (filter (lambda (k) (not (key=? a-key k))) keys)])))

(define (start-scene)
  (big-bang (gamestate (list (bugger '(0 0) '(0 0) (circle 20 100 "blue")))
                       '())
            (on-tick update-game)
            (on-key keydown)
            (on-release keyup)
            (to-draw render-game)))

(start-scene)