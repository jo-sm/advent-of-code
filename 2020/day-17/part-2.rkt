#lang racket/base

(require racket/file
         racket/list
         racket/set
         rackunit)

(struct point (x y z w) #:transparent)

(define (surrounding-coords coord)
  (for*/fold ([result (set)] #:result (set-remove result coord))
             ([x-coord (range (- (point-x coord) 1) (+ (point-x coord) 2))]
              [y-coord (range (- (point-y coord) 1) (+ (point-y coord) 2))]
              [z-coord (range (- (point-z coord) 1) (+ (point-z coord) 2))]
              [w-coord (range (- (point-w coord) 1) (+ (point-w coord) 2))])
    (set-add result (point x-coord y-coord z-coord w-coord))))

(define (get-all-points active-points)
  (define others (foldl set-union (set) (set-map active-points surrounding-coords)))

  (set-union (set) active-points others))

(define (new-active-coords active-coords coord)
  (define is-currently-active (set-member? active-coords coord))
  (define active-surrounding (set-intersect active-coords (surrounding-coords coord)))

  (if is-currently-active
    (and (<= 2 (set-count active-surrounding) 3)
         coord)
    (and (= (set-count active-surrounding) 3)
         coord)))

(define (get-initial-active-coords file)
  (let ([lines (file->lines file)])
    (for/fold ([result (set)])
      ([y (range 0 (length lines))])
      (let* ([line (string->list (list-ref lines y))]
             [active-x-coords (indexes-where line (lambda (c) (eq? c #\#)))])
        (foldl (lambda (x result) (set-add result (point x y 0 0))) result active-x-coords)))))

(define (run initial-active-coords n)
  (set-count (for/fold ([active initial-active-coords])
               ([i (range 0 n)])
               (let ([all-coords (get-all-points active)])
                 (foldl (lambda (p memo)
                          (if p
                            (set-add memo p)
                            memo))
                        (set)
                        (set-map all-coords (lambda (coord) (new-active-coords active coord))))))))

; real time: ~1500 (surround list)
; real time: ~1600 (all set)
(check-eq? (time (run (get-initial-active-coords "example") 6)) 848)

; real time: ~3400 (surround list)
; real time: ~3500 (all set)
(check-eq? (time (run (get-initial-active-coords "input") 6)) 2332)
