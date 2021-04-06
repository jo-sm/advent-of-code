#lang racket/base

(require rackunit)

(define (play m current-number result n)
  (define next-current-number
    (if (vector-ref result current-number) (- n 1 (vector-ref result current-number)) 0))

  (vector-set! result current-number (- n 1))

  (if (= m n) current-number (play m next-current-number result (+ n 1))))

(define n 30000000)

; '(0 3 6)
(define example-vector (make-vector n #f))
(vector-set! example-vector 0 0)
(vector-set! example-vector 3 1)
(vector-set! example-vector 6 2)

; '(20 9 11 0 1 2)
(define input-vector (make-vector n #f))
(vector-set! input-vector 20 0)
(vector-set! input-vector 9 1)
(vector-set! input-vector 11 2)
(vector-set! input-vector 0 3)
(vector-set! input-vector 1 4)
(vector-set! input-vector 2 5)

(check-eq? (play n 0 example-vector 4) 175594)

(check-eq? (play n 0 input-vector 7) 48568)
