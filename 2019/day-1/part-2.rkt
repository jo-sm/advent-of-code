#lang racket

(require "../utils.rkt")

(define (module-fuel-req mass)
  (define (iter total rem-mass)
    (let ([next (- (floor (/ rem-mass 3)) 2)])
      (if (<= next 0)
        total
        (iter (+ total next) next)
      )
    )
  )

  (iter 0 mass)
)

(apply + (map module-fuel-req (read "fuel-input.txt" string->number)))
