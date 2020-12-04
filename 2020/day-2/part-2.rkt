#lang racket

(require "../utils.rkt")

(define (parse-line line)
  (let* (
    [split (string-split line)]
    [numbers (map string->number (string-split (list-ref split 0) "-"))]
    [i (list-ref numbers 0)]
    [j (list-ref numbers 1)]
    [char (list-ref (string-split (list-ref split 1) ":") 0)]
    [password-chars (string-split (list-ref split 2) "")]
  )
    (xor (equal? (list-ref password-chars i) char) (equal? (list-ref password-chars j) char))
  )
)

(define (main)
  (length (filter (lambda (x) x) (read-input-lines #:line-parser parse-line)))
)

(main)
