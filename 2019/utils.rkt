#lang racket

(define (read filename [mapf (lambda (x) x)])
  (define lines (port->lines (open-input-file filename #:mode 'text)))

  (map mapf lines)
)

(define (add-pair pair)
  (+ (car pair) (cdr pair))
)

(provide read)
(provide add-pair)
