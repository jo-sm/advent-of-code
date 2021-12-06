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

(define (get-path directions [path (list (coord 0.0 0.0))] [current-heading 90])
  (define current-direction (first directions))

  (define new-heading
    (case (first current-direction)
      [(#\R) (+ current-heading (second current-direction))]
      [(#\L) (- current-heading (second current-direction))]
      [else current-heading]))

  (define x-incr
    (case (first current-direction)
      [(#\E) 1]
      [(#\W) -1]
      [(#\N #\S) 0]
      [(#\F) (inexact->exact (sin (degrees->radians new-heading)))]
      [else 0]))
  (define y-incr
    (case (first current-direction)
      [(#\N) 1]
      [(#\S) -1]
      [(#\E #\W) 0]
      [(#\F) (inexact->exact (cos (degrees->radians new-heading)))]
      [else 0]))

  (cond
    [(null? (cdr directions)) (apply-direction-to-path path current-direction x-incr y-incr)]
    [(and (= x-incr 0)
          (= y-incr 0))
     (get-path (cdr directions) path new-heading)]
    [else (get-path (cdr directions)
                    (apply-direction-to-path path current-direction x-incr y-incr)
                    new-heading)]))

(check-equal? (calc-rectilinear-distance
               (last (get-path (read-input-lines "example" #:line-parser line->directions))))
              25.0)

(calc-rectilinear-distance (last (get-path (read-input-lines #:line-parser line->directions))))
