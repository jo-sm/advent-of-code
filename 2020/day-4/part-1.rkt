#lang racket

; Pretty simple - the trickiest part was thinking about the best way to do the lambda
; that turns "byr:1992" into ("byr" "1992").

(require "../utils.rkt")

(define (is-valid-passport passport)
  (define required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

  (define (iter rem-keys rem-passport)
    (cond
      ((and (= (length rem-passport) 0) (> (length rem-keys) 0)) false)
      ((and (= (length rem-passport) 0) (= (length rem-keys) 0)) true)
      (else (iter (remove (first (car rem-passport)) rem-keys) (cdr rem-passport)))
    )
  )

  (iter required-keys passport)
)

(define passports
  (map
    (lambda (x) (map
      (lambda (y) (string-split y ":"))
      x)
    )
    (map
      string-split
      (read-input-file #:file-parser (lambda (f) (string-split f "\n\n")))
    )
  )
)

(length (filter is-valid-passport passports))
