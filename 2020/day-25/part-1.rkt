#lang racket/base

(require racket/list)

(define (transform-subject-number subject-number times)
  (for/fold ([result 1])
    ([i (range 0 times)])
    (remainder (* result subject-number) 20201227)))

(define (find-loop-size public-key subject-number [value 1] [loop-count 0])
  (if (= value public-key)
    loop-count
    (find-loop-size public-key
                    subject-number
                    (remainder (* value subject-number) 20201227)
                    (+ loop-count 1))))

(transform-subject-number 17807724 (find-loop-size 8335663 7))
