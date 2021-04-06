#lang racket/base

(require "../utils.rkt")
(require racket/list)
(require racket/math)
(require rackunit)

(struct coord (x y) #:transparent)

(define (line->directions line)
  (list (string-ref line 0) (string->number (substring line 1))))

(define (apply-direction-to-path path current-direction x-incr y-incr)
  (define last-coord (last path))

  (if (= (second current-direction) 0)
      path
      (apply-direction-to-path
       (append path (list (coord (+ (coord-x last-coord) x-incr) (+ (coord-y last-coord) y-incr))))
       (list (first current-direction) (- (second current-direction) 1))
       x-incr
       y-incr)))

(define (calc-rectilinear-distance coord)
  (+ (abs (coord-x coord)) (abs (coord-y coord))))

(define (get-path directions
                  [path (list (coord 0.0 0.0))]
                  [current-heading 0]
                  [current-waypoint (coord 10.0 1.0)])
  (define current-direction (first directions))

  ; Rotation is from Q1 to Q2, so it increases as we rotate CCW (meaning, going right or CW decreases it)
  (define new-heading-amount
    (case (first current-direction)
      [(#\R) (* -1 (second current-direction))]
      [(#\L) (second current-direction)]
      [else 0]))
  (displayln new-heading-amount)

  (define new-waypoint
    (cond
      [(equal? (first current-direction) #\E)
       (coord (+ (coord-x current-waypoint) (second current-direction)) (coord-y current-waypoint))]
      [(equal? (first current-direction) #\W)
       (coord (- (coord-x current-waypoint) (second current-direction)) (coord-y current-waypoint))]
      [(equal? (first current-direction) #\N)
       (coord (coord-x current-waypoint) (+ (coord-y current-waypoint) (second current-direction)))]
      [(equal? (first current-direction) #\S)
       (coord (coord-x current-waypoint) (- (coord-y current-waypoint) (second current-direction)))]
      [(= new-heading-amount 0) current-waypoint]
      [else (coord
             (round (- (* (coord-x current-waypoint) (cos (degrees->radians new-heading-amount)))
                       (* (coord-y current-waypoint) (sin (degrees->radians new-heading-amount)))))
             (round (+ (* (coord-x current-waypoint) (sin (degrees->radians new-heading-amount)))
                       (* (coord-y current-waypoint)
                          (cos (degrees->radians new-heading-amount))))))]))

  (define x-incr
    (case (first current-direction)
      [(#\F) (coord-x new-waypoint)]
      [else 0]))

  (define y-incr
    (case (first current-direction)
      [(#\F) (coord-y new-waypoint)]
      [else 0]))

  (cond
    [(null? (cdr directions)) (apply-direction-to-path path current-direction x-incr y-incr)]
    [(and (= x-incr 0) (= y-incr 0))
     (get-path (cdr directions) path (+ current-heading new-heading-amount) new-waypoint)]
    [else (get-path (cdr directions)
                    (apply-direction-to-path path current-direction x-incr y-incr)
                    (+ current-heading new-heading-amount)
                    new-waypoint)]))

(check-equal? (calc-rectilinear-distance
               (last (get-path (read-input-lines "example" #:line-parser line->directions))))
              286.0)

(calc-rectilinear-distance (last (get-path (read-input-lines #:line-parser line->directions))))
