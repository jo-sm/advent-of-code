#lang racket

(define (read-file [filename "input"] [processor identity])
  (processor (port->string (open-input-file filename #:mode 'text)))
)

(provide read-file)

(define (read [filename "input"] [mapf identity])
  (map
    mapf
    (read-file filename (lambda (f) (string-split f "\n")))
  )
)

(provide read)

(define (list->values lst)
  (apply values lst)
)

(provide list->values)
