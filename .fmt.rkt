#lang racket/base

(provide the-formatter-map)
(require fmt/conventions)

(define (the-formatter-map s)
  (case s
    [("define/contract") (format-define-like)]
    [("~>" "~>>") format-require]
    [("or" "and") format-require]
    [("if" "findf" "λ" "for/fold" "for*/fold") (format-uniform-body/helper 1)]
    [else #f]))
