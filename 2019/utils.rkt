#lang racket

(define (read filename mapf)
  (define lines (port->lines (open-input-file filename #:mode 'text)))

  (map mapf lines)
)

(define (add-pair pair)
  (+ (car pair) (cdr pair))
)

(provide read)
(provide add-pair)
