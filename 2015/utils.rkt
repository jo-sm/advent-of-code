#lang racket/base

(require racket/contract
         racket/function
         racket/port)

(provide (all-defined-out))

(define/contract (read-input-file [filename "input"] #:file-parser [file-parser identity])
                 (->* () (string? #:file-parser (-> string? any/c)) any/c)
                 (file-parser (port->string (open-input-file filename #:mode 'text))))
