#lang racket

(define (read filename [mapf (lambda (x) x)])
  (define lines (port->lines (open-input-file filename #:mode 'text)))

  (map mapf lines)
)

(provide read)

(define (read-file filename)
  (port->string (open-input-file filename #:mode 'text))
)

(provide read-file)
