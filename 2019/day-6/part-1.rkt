#lang racket

(require "../utils.rkt")

(define orbits (read "input" (lambda (line) (string-split line ")"))))
(define COM (findf (lambda (i) (equal? (first i) "COM")) orbits))

(define (generate-path end start lst)
  (define (iter result)
    (if (equal? (first result) end)
        result
        (iter (cons (findf (lambda (i) (equal? (first (first result)) (second i))) lst) result))))

  (iter (list start)))

(foldr (lambda (i memo) (+ (length (generate-path COM i orbits)) memo)) 0 orbits)
