#lang racket

(define (read-fuel-input)
  (define io (open-input-file "fuel-input.txt" #:mode 'text))

  ; Iterate over each line until eof. For each line, convert the read string into a number
  ; Since order isn't important for this exercise, it is reverse of the order in `fuel-input.txt`
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

(define (calc-fuel-req-for-mass mass) (- (floor (/ mass 3)) 2))

(define (sum-fuel-reqs)
  ; Add each fuel requirement together until there is nothing left in the list
  ; Fuel req. calc: f(mass) = [[mass / 3]] - 2

  (foldr (lambda (i memo) (+ memo (calc-fuel-req-for-mass i))) 0 (read-fuel-input))
)

(sum-fuel-reqs)
