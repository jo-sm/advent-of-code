#lang racket

(require "../utils.rkt")

(struct seat (row col))
(define (seat-id seat)
  (+ (* (seat-row seat) 8) (seat-col seat))
)

(define (line-to-ops line)
  (list
    (string->list (substring line 0 7))
    (string->list (substring line 7))
  )
)

(define (apply-op op nums)
  (if (index-of '(#\F #\L) op)
    (take nums (/ (length nums) 2))
    (drop nums (/ (length nums) 2))
  )
)

(define (get-row-col ops)
  (seat
    (first (foldl (lambda (op rows) (apply-op op rows)) (range 0 128) (first ops)))
    (first (foldl (lambda (op cols) (apply-op op cols)) (range 0 8) (second ops)))
  )
)

(first (sort
  (map
    seat-id
    (map
      get-row-col
      (read-input-lines #:line-parser line-to-ops)
    )
  )
  >
))
