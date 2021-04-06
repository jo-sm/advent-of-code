#lang racket

(require "../utils.rkt")

(define (is-valid-password line)
  (define-values (raw-expected-positions raw-char password) (apply values (string-split line)))

  (define-values (pos-i pos-j)
    (apply values (map string->number (string-split raw-expected-positions "-"))))

  (define expected-char (string-ref raw-char 0))

  ; `expected-char` must be at either `pos-i` or `pos-j`, but not both
  (xor (equal? (string-ref password (- pos-i 1)) expected-char)
       (equal? (string-ref password (- pos-j 1)) expected-char)))

(length (filter is-valid-password (read-input-lines)))
