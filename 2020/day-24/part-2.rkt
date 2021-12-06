#lang racket/base

(require racket/file
         racket/match
         racket/sequence
         racket/set)

(define (find-next-token str-port)
  (match (regexp-match #px"^se|nw|sw|ne|w|e" str-port)
    [#f eof]
    [(list str) (bytes->string/utf-8 str)]))

(define parts
  (for/list ([line (file->lines "input")])
    (sequence->list (in-port find-next-token (open-input-string line)))))

(define (next-point direction x y)
  (case direction
    [("se") (cons (+ x 0.5) (+ y 1))]
    [("sw") (cons (- x 0.5) (+ y 1))]
    [("ne") (cons (+ x 0.5) (- y 1))]
    [("nw") (cons (- x 0.5) (- y 1))]
    [("e") (cons (+ x 1) y)]
    [("w") (cons (- x 1) y)]))

(define (adjacent-tiles tile)
  (set (next-point "se" (car tile) (cdr tile))
       (next-point "sw" (car tile) (cdr tile))
       (next-point "ne" (car tile) (cdr tile))
       (next-point "nw" (car tile) (cdr tile))
       (next-point "e" (car tile) (cdr tile))
       (next-point "w" (car tile) (cdr tile))))

(define (run current-black-tiles remaining-iters)
  (define all-tiles
    (foldl (λ (tile memo)
             (set-union memo (adjacent-tiles tile)))
           (set)
           (set->list current-black-tiles)))

  (if (= remaining-iters 0)
    current-black-tiles
    (run (for/fold ([next-black-tiles (set)])
           ([tile all-tiles])
           (let ([num-adjacent-black-tiles
                  (set-count (set-intersect (adjacent-tiles tile) current-black-tiles))])
             (if (set-member? current-black-tiles tile)
               (if (< 0 num-adjacent-black-tiles 3)
                 (set-add next-black-tiles tile)
                 next-black-tiles)
               (if (= num-adjacent-black-tiles 2)
                 (set-add next-black-tiles tile)
                 next-black-tiles))))
         (- remaining-iters 1))))

(define starting-black-tiles
  (for/fold ([result (set)])
    ([directions parts])
    (let ([tile (for/fold ([x 0.0] [y 0.0] #:result (cons x y))
                  ([dir directions])
                  (let ([next (next-point dir x y)]) (values (car next) (cdr next))))])

      (if (set-member? result tile)
        (set-remove result tile)
        (set-add result tile)))))

(set-count (run starting-black-tiles 100))
