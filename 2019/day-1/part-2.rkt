#lang racket

(define (read-fuel-input)
  (define io (open-input-file "fuel-input.txt" #:mode 'text))

  (define (iter i)
    (let ([line (read-line io)])
      (if (equal? eof line)
        i
        (iter (cons (string->number line) i))
      )
    )
  )

  (iter '())
)

(define (calc-fuel-req-for-module total mass)
  ; If the calculation is less than 0, do not add it to the result, and return
  ; the result. Otherwise, add it and continue the iteration.
  ; Fuel req calc: [[i/3]] - 2
  (let ([next (- (floor (/ mass 3)) 2)])
    (if (<= next 0)
      total
      (calc-fuel-req-for-module (+ total next) next)
    )
  )
)

(define (sum-fuel-reqs)
  (foldr (lambda (i memo) (+ memo (calc-fuel-req-for-module 0 i))) 0 (read-fuel-input))
)

(sum-fuel-reqs)
