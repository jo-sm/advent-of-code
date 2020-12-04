#lang racket

(require "../utils.rkt")

(define (get-num-trees lines)
  (define line-length (string-length (list-ref lines 0))) ; all lines are the same size

  (define (iter num-trees x y)
    (cond
      ((> y (- (length lines) 1)) num-trees)
      ((equal? (string-ref (list-ref lines y) (modulo x line-length)) #\#) (iter (+ 1 num-trees) (+ x 3) (+ y 1)))
      (else (iter num-trees (+ x 3) (+ y 1)))
    )
  )

  (iter 0 3 1)
)

(get-num-trees (read "input" identity))
