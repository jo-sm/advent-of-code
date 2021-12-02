#lang racket/base

(require racket/contract
         racket/function
         racket/port
         racket/string
         srfi/26)

(provide (all-defined-out))

(define/contract (read-input-file [filename "input"] #:file-parser [file-parser identity])
                 (->* () (string? #:file-parser (-> string? any/c)) any/c)
                 (file-parser (port->string (open-input-file filename #:mode 'text))))

; Read each line and apply the given #:line-parser function to it
(define/contract
 (read-input-lines [filename "input"] #:line-parser [line-parser identity])
 (->* () (string? #:line-parser (-> string? any/c)) any/c)
 (map line-parser (read-input-file filename #:file-parser (cut string-split <> "\n"))))
