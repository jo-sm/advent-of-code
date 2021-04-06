#lang racket/base

(require "../utils.rkt"
         srfi/26
         racket/string
         racket/list)

(define (is-valid-password line)
  (define-values (raw-range raw-char password) (apply values (string-split line)))

  (define-values (min max) (apply values (map string->number (string-split raw-range "-"))))

  (define expected-char (string-ref raw-char 0))
  (define times-char-in-password (count (cut equal? <> expected-char) (string->list password)))

  (and (>= times-char-in-password min) (<= times-char-in-password max)))

(length (filter is-valid-password (read-input-lines)))
