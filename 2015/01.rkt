#lang racket/base

(require "./utils.rkt"
         rackunit)

(define (part-1 input)
  (for/fold ([floor 0])
    ([char input] #:when (or (eq? #\( char)
                             (eq? #\) char)))
    (cond
      [(eq? #\( char) (+ floor 1)]
      [(eq? #\) char) (- floor 1)])))

(define (part-2 input)
  ; We need to get the position _as_ Santa enters the basement, not when he is already in the basement, so we subtract 1
  ; since it happened in the last iteration
  (for/fold ([floor 0] [pos 1] #:result (- pos 1))
    ([char input] #:when (or (eq? #\( char)
                             (eq? #\) char))
                  #:break (= -1 floor))
    (cond
      [(eq? #\( char) (values (+ floor 1) (+ pos 1))]
      [(eq? #\) char) (values (- floor 1) (+ pos 1))])))

(define example-1 (string->list ")())())"))
(define example-2 (string->list "))((((("))
(define example-3 (string->list "()())"))
(define input (read-input-file "01.rktd" #:file-parser string->list))

(check-eq? (part-1 example-1) -3)
(check-eq? (part-1 example-2) 3)
(check-eq? (part-1 input) 232)

(check-eq? (part-2 example-3) 5)
(check-eq? (part-2 input) 1783)
