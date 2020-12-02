#lang racket

(require "../utils.rkt")

(define (parse-line line)
  (let* (
    [split (string-split line)]
    [numbers (map string->number (string-split (list-ref split 0) "-"))]
    [min (list-ref numbers 0)]
    [max (list-ref numbers 1)]
    [char (list-ref (string-split (list-ref split 1) ":") 0)]
    [num (length (filter (lambda (x) (equal? x char)) (string-split (list-ref split 2) "")))]
  )
    (and (>= num min) (<= num max))
  )
)

(define (main)
  (length (filter (lambda (x) x) (read "input" parse-line)))
)

(main)
