#lang racket

(require "../utils.rkt")
(require "../Intcode/runner.rkt")

(define program-raw
  (flatten (read "Intcode.program"
                 (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))
(define program (list-set (list-set program-raw 1 12) 2 2))

(first (second (start program)))
