#lang racket

(require "../utils.rkt")

(struct range (min max))

(define (is-valid-password line)
  (let*-values (
    [(raw-range raw-char password) (list->values (string-split line))]
    [(pass-range) (apply range (map string->number (string-split raw-range "-")))]
    [(char) (string-ref raw-char 0)]
    [(num) (count (lambda (x) (equal? x char)) (string->list password))]
  )
    (and (>= num (range-min pass-range)) (<= num (range-max pass-range)))
  )
)

(length (filter is-valid-password (read-input-lines)))
