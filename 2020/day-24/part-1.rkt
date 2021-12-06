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
    [("se") (values (+ x 0.5) (+ y 1))]
    [("sw") (values (- x 0.5) (+ y 1))]
    [("ne") (values (+ x 0.5) (- y 1))]
    [("nw") (values (- x 0.5) (- y 1))]
    [("e") (values (+ x 1) y)]
    [("w") (values (- x 1) y)]))

(set-count (for/fold ([result (set)])
             ([directions parts])
             (let ([tile (for/fold ([x 0.0] [y 0.0] #:result (cons x y))
                           ([dir directions])
                           (next-point dir x y))])

               (if (set-member? result tile)
                 (set-remove result tile)
                 (set-add result tile)))))
