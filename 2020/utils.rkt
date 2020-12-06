#lang racket

(provide read-input-file)
(provide read-input-lines)
(provide truthy?)
(provide largest)
(provide smallest)

(require srfi/26)

; Read the file and apply the given #:file-parser to it
(define/contract (read-input-file [filename "input"] #:file-parser [file-parser identity])
  (->* () (string? #:file-parser (-> string? any/c)) any/c)

  (file-parser (port->string (open-input-file filename #:mode 'text)))
)

; Read each line and apply the given #:line-parser function to it
(define/contract (read-input-lines [filename "input"] #:line-parser [line-parser identity])
  (->* () (string? #:line-parser (-> string? any/c)) any/c)

  (map
    line-parser
    (read-input-file filename #:file-parser (cut string-split <> "\n"))
  )
)

; Convenience function for checking if something is not falsy.
; Useful for filtering.
(define/contract (truthy? v)
  (-> any/c boolean?)

  (not (not v))
)

(define/contract (largest numbers)
  (-> (listof number?) number?)

  (first (sort numbers >))
)

(define/contract (smallest numbers)
  (-> (listof number?) number?)

  (first (sort numbers <))
)
