#lang racket

(require "../utils.rkt")

(define (list-head lst)
  (reverse (rest (reverse lst)))
)

(define (remove-second lst)
  (cons (car lst) (cdr (cdr lst)))
)

(define (find-n n lst)
  (define (iter remaining)
    (cond
      ((< (+ (car remaining) (last remaining) (second remaining)) n) (iter (remove-second remaining)))
      ((> (+ (car remaining) (last remaining) (second remaining)) n) (iter (list-head remaining)))
      (else (list (car remaining) (last remaining) (second remaining)))
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
