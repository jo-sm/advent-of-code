#lang racket/base

(require racket/list)
(require srfi/26)

(define (play n numbers [result '()])
  (cond
    [(= n 0) result]
    [(> (length numbers) 0) (play (- n 1) (cdr numbers) (cons (car numbers) result))]
    [(= (count (cut = <> (car result)) result) 1) (play (- n 1) numbers (cons 0 result))]
    [else (play (- n 1) numbers (cons (+ (index-of (cdr result) (car result)) 1) result))]))

(define example '(0 3 6))
(define input '(20 9 11 0 1 2))

(car (play 2020 example))
(car (play 2020 input))
