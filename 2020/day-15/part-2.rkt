#lang racket/base

#|

Learned about vectors today! Doing this but with vectors (see vector.rkt)
reduces the amount of time from ~7s to ~2s, with approx 500ms of gc in either case
(slightly more for the vector approach).

Don't even talk to me or my son about immutable hashes.

|#

(define (play m current-number result n)
  (define next-current-number (- n 1 (hash-ref result current-number (- n 1))))

  (hash-set! result current-number (- n 1))

  (if (= m n)
    current-number
    (play m next-current-number result (+ n 1))))

(define example '(0 3 6))
(define input '(20 9 11 0 1 2))

(play 30000000 0 (make-hash (list (cons 0 0) (cons 3 1) (cons 6 2))) 4)
; (check-eq?
;   175594))

; (check-eq?
;   (play 30000000 0 (make-hash (list (cons 20 0) (cons 9 1) (cons 11 2) (cons 0 3) (cons 1 4) (cons 2 5))) 7)
;   48568)
