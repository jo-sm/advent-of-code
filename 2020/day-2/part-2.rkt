#lang racket

(require "../utils.rkt")

(struct pos (i j))

(define (is-valid-password line)
  (let*-values (
    [(raw-pos raw-char password) (list->values (string-split line))]
    [(positions) (apply pos (map string->number (string-split raw-pos "-")))]
    [(char) (string-ref raw-char 0)]
  )
    (xor
      (equal? (string-ref password (- (pos-i positions) 1)) char)
      (equal? (string-ref password (- (pos-j positions) 1)) char)
    )
  )
)

(length (filter is-valid-password (read-input-lines)))
