#lang racket

(require "../utils.rkt")

(define (list-head lst)
  (reverse (rest (reverse lst)))
)

(define (remove-second lst)
  (cons (car lst) (cddr lst))
)

; Requires lst to be in ascending order
(define (find-n n lst)
  (define (iter remaining)
    (cond
      ; Remove 2nd element from remaining if 1st + 2nd + last are less than n
      ((< (+ (car remaining) (last remaining) (second remaining)) n) (iter (remove-second remaining)))

      ; Remove last element from remaining if 1st + 2nd + last are greater than n
      ((> (+ (car remaining) (last remaining) (second remaining)) n) (iter (list-head remaining)))

      ; Return 1st, 2nd, and last elements
      (else (list (car remaining) (last remaining) (second remaining)))
    )
  )

  (iter lst)
)

(define asc-numbers (sort (read-input-lines #:line-parser string->number) <))

(apply * (find-n 2020 asc-numbers))
