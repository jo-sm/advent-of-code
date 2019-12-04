#lang racket

(define password-range (range 128392 643281))

(define (char->number c)
  (- (char->integer c) (char->integer #\0))
)

(define (always-increasing password)
  (define (iter result digit remaining-password)
    (cond
      ((null? remaining-password) result)
      ((>= (car remaining-password) digit) (iter true (car remaining-password) (cdr remaining-password)))
      (else (iter false 0 null))
    )
  )

  (iter true (car password) (cdr password))
)

(define (has-duplicate password)
  (define (iter result digit remaining-password)
    (cond
      ((or (equal? result true) (null? remaining-password)) result)
      ((= digit (car remaining-password)) (iter true (car remaining-password) (cdr remaining-password)))
      (else (iter false (car remaining-password) (cdr remaining-password)))
    )
  )

  (iter false (car password) (cdr password))
)

(define (is-valid-password i)
  (define pieces (map char->number (string->list (number->string i))))

  (and (always-increasing pieces) (has-duplicate pieces))
)

(length (filter is-valid-password password-range))
