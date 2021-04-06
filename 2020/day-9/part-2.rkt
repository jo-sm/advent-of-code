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

(define (find-contiguous-numbers numbers i)
  (define (iter start-numbers-pos current-pos)
    (define check-nums (take-between numbers start-numbers-pos current-pos))
    (define checksum (apply + check-nums))

    (cond
      [(= checksum i) check-nums]
      [(> checksum i) (iter (+ start-numbers-pos 1) (+ start-numbers-pos 2))]
      [else (iter start-numbers-pos (+ current-pos 1))]))

  (iter 1 2))

(define (get-weakness-number rnge)
  (define sorted (sort rnge <))

  (+ (first sorted) (last sorted)))

(define example (read-input-lines "example" #:line-parser string->number))
(define input (read-input-lines #:line-parser string->number))

(check-eq? (get-weakness-number (find-contiguous-numbers example (find-invalid-number 5 example))) 62)

(get-weakness-number (find-contiguous-numbers input (find-invalid-number 25 input)))
