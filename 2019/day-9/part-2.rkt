#lang racket

(require "../utils.rkt")
(require "../Intcode/constants.rkt")
(require "../Intcode/runner.rkt")

(define program (flatten (read "Intcode.program" (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(last (fourth (run-intcode-program program #:input 2)))
