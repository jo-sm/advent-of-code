#lang racket

(require "../utils.rkt")

(define lines (read-input-lines))

(define (get-num-trees x-length y-length)
  (define line-length (string-length (list-ref lines 0))) ; all lines are the same size

  (define (iter num-trees x y)
    (cond
      ((> y (- (length lines) 1)) num-trees)
      ((equal? (string-ref (list-ref lines y) (modulo x line-length)) #\#) (iter (+ 1 num-trees) (+ x x-length) (+ y y-length)))
      (else (iter num-trees (+ x x-length) (+ y y-length)))
    )
  )

  (iter 0 x-length y-length)
)

(*
  (get-num-trees 1 1)
  (get-num-trees 3 1)
  (get-num-trees 5 1)
  (get-num-trees 7 1)
  (get-num-trees 1 2)
)