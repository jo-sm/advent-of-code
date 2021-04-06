#lang racket/base

(require racket/list)
(require racket/sequence)
(require "../utils.rkt")
(require rackunit)

(define (take-between lst start end)
  (take (drop lst start) (- end start -1)))

(define (number-is-valid check-numbers number)
  (define possible-combinations (in-combinations check-numbers 2))
  (define found-combos
    (sequence-filter (lambda (combo) (= (apply + combo) number)) possible-combinations))

  (> (sequence-length found-combos) 0))

(define (find-invalid-number preamble-length numbers)
  (define (iter pos)
    (define check-numbers-list (take-between numbers pos (+ preamble-length pos)))
    (define current-number (list-ref numbers (+ preamble-length pos)))

    (cond
      [(null? current-number) null]
      [(number-is-valid check-numbers-list current-number) (iter (+ pos 1))]
      [else current-number]))

  (iter 0))

(check-eq? (find-invalid-number 5 (read-input-lines "example" #:line-parser string->number)) 127)

(find-invalid-number 25 (read-input-lines #:line-parser string->number))
