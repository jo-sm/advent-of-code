#lang racket

(define password-range (range 128392 643281))

(define (number->digits number)
  (define (split-into-digits digits remaining-number)
    (define-values (next-remaining-number digit) (quotient/remainder remaining-number 10))

    (if (= next-remaining-number 0)
      ; The quotient is 0, meaning that the remainder is the final digit
      ; Return this digit + the previous ones
      (cons digit digits)

      ; The quotient isn't 0, continue
      (split-into-digits (cons digit digits) next-remaining-number)))

  (split-into-digits '() number))

(define (always-increasing password)
  (equal? (sort password <) password))

(define (has-duplicate password)
  (define (iter remaining-password)
    (cond
      [(= (length remaining-password) 1) false]
      [(= (car remaining-password) (cadr remaining-password)) true]
      [else (iter (cdr remaining-password))]))

  (iter password))

(define (is-valid-password i)
  (define pieces (number->digits i))

  (and (always-increasing pieces)
       (has-duplicate pieces)))

(length (filter is-valid-password password-range))
