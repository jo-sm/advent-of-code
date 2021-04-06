#lang racket/base

(require racket/list
         racket/vector
         racket/function)

(define size 1000001)

(define (play next-elements remaining-rounds current-cup)
  (define destination-cup (- current-cup 1))
  (define p1 (vector-ref next-elements current-cup))
  (define p2 (vector-ref next-elements (vector-ref next-elements current-cup)))
  (define p3
    (vector-ref next-elements (vector-ref next-elements (vector-ref next-elements current-cup))))

  (if (= remaining-rounds 0)
      next-elements
      (let loop ([destination destination-cup])
        (cond
          [(< destination 1) (loop (- size 1))]
          [(or (= destination p1) (= destination p2) (= destination p3)) (loop (- destination 1))]
          [else (let ([a (vector-ref next-elements destination)]
                      [b (vector-ref next-elements current-cup)]
                      [c (vector-ref next-elements p3)])
                  (vector-set! next-elements p3 a)
                  (vector-set! next-elements destination b)
                  (vector-set! next-elements current-cup c)

                  (play next-elements (- remaining-rounds 1) c))]))))

(define (make-arrangement start size)
  (define arrangement (make-vector size #f))
  (define next-elements (make-vector size #f))

  (for ([i (in-range 0 size)])
    (vector-set! arrangement i (+ i 1)))
  (for ([i (in-range 1 size)])
    (vector-set! next-elements i (+ i 2)))

  (define initial-arrangement
    (map (λ (n-char) (string->number (string n-char))) (string->list start)))

  (for ([(num i) (in-indexed initial-arrangement)])
    (vector-set! arrangement i num))

  (for ([i (in-range (- (vector-length arrangement) 1))])
    (vector-set! next-elements (vector-ref arrangement i) (vector-ref arrangement (+ i 1))))

  (vector-set! next-elements (vector-memq size next-elements) (vector-ref arrangement 0))
  (vector-set! next-elements 0 (vector-ref arrangement 0))

  next-elements)

(define next-elements (make-arrangement "198753462" size))
(define result (play next-elements 10000000 (vector-ref next-elements 0)))

(* (vector-ref result 1) (vector-ref result (vector-ref result 1)))
