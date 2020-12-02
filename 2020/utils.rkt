#lang racket

(define (read filename [mapf (lambda (x) x)])
  (define lines (port->lines (open-input-file filename #:mode 'text)))

  (map mapf lines)
)

(provide read)
