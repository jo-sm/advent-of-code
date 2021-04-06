#lang racket

(require srfi/26)
(provide (all-defined-out))

; Read the file and apply the given #:file-parser to it
(define/contract (read-input-file [filename "input"] #:file-parser [file-parser identity])
                 (->* () (string? #:file-parser (-> string? any/c)) any/c)
                 (file-parser (port->string (open-input-file filename #:mode 'text))))

; Read each line and apply the given #:line-parser function to it
(define/contract
 (read-input-lines [filename "input"] #:line-parser [line-parser identity])
 (->* () (string? #:line-parser (-> string? any/c)) any/c)
 (map line-parser (read-input-file filename #:file-parser (cut string-split <> "\n"))))

; Convenience function for checking if something is not falsy.
; Useful for filtering.
(define/contract (truthy? v) (-> any/c boolean?) (not (not v)))

; Return the largest number in the list
(define/contract (largest numbers) (-> (listof number?) number?) (first (sort numbers >)))

; Return the smallest number in the list
(define/contract (smallest numbers) (-> (listof number?) number?) (first (sort numbers <)))

; list-ref, but returns null instead of a contract violation if the given position would
; be too small or large for the list.
(define/contract (list-ref-safe lst i)
                 (-> (listof any/c) number? (or/c any/c null?))
                 (if (or (>= i (length lst)) (< i 0)) null (list-ref lst i)))
