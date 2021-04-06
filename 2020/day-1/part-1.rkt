#lang racket

(require "../utils.rkt")

(define (list-head lst)
  (reverse (rest (reverse lst))))

; Requires lst to be in ascending order
(define (find-n n lst)
  (define (iter remaining)
    (cond
      ; Remove first element from remaining if 1st + last is too small
      [(< (+ (car remaining) (last remaining)) n) (iter (cdr remaining))]

      ; Remove last element if 1st + last is too large
      [(> (+ (car remaining) (last remaining)) n) (iter (list-head remaining))]

      ; The first and last add up to be `n`
      [else (list (car remaining) (last remaining))]))

  (iter lst))

(define asc-numbers (sort (read-input-lines #:line-parser string->number) <))

(apply * (find-n 2020 asc-numbers))
