#lang racket

(require "../utils.rkt")

(define (list-head lst)
  (reverse (rest (reverse lst)))
)

; Requires lst to be in ascending order
(define (find-n n lst)
  (define (iter remaining)
    (cond
      ((< (+ (car remaining) (last remaining)) n) (iter (cdr remaining)))
      ((> (+ (car remaining) (last remaining)) n) (iter (list-head remaining)))
      (else (list (car remaining) (last remaining)))
    )
  )

  (iter lst)
)

(apply
  *
  (find-n
    2020
    (sort (read-input-lines #:line-parser string->number) <)
  )
)
