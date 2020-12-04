#lang racket

(define (read-input-file [filename "input"] #:file-parser [file-parser identity])
  (file-parser (port->string (open-input-file filename #:mode 'text)))
)

(provide read-input-file)

(define (read-input-lines [filename "input"] #:line-parser [line-parser identity])
  (map
    line-parser
    (read-input-file filename #:file-parser (lambda (f) (string-split f "\n")))
  )
)

(provide read-input-lines)

(define (list->values lst)
  (apply values lst)
)

(provide list->values)
