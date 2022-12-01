#lang racket/base

(require "../../utils.rkt"
         racket/list
         racket/string
         rackunit
         threading)

(define (part-1 groups)
  (~>> groups
       (map (lambda (group) (apply + group)))
       (sort _ >)
       car))

(define (part-2 groups)
  (~>> groups
       (map (lambda (group) (apply + group)))
       (sort _ >)
       (take _ 3)
       (apply +)))

(define (parser raw)
  (~>> raw
       (string-split _ "\n\n")
       (map (lambda (group) (string-split group "\n")))
       (mapmap string->number)))

(define example (parser "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"))
(define input (parse "input" #:parser parser))

(check-eq? (part-1 example) 24000)
(check-eq? (part-1 input) 70116)

(check-eq? (part-2 example) 45000)
(check-eq? (part-2 input) 206582)
