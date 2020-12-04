#lang racket

; Pretty simple - the trickiest part was thinking about the best way to do the lambda
; that turns "byr:1992" into ("byr" "1992").

(require "../utils.rkt")

(define passports
  (map (lambda (x) (map (lambda (y) (string-split y ":")) x)) (map string-split (string-split (read-file "input") "\n\n")))
)

(define (valid-passport passport)
  (define required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

  (define (iter remaining-keys remaining-passport)
    (cond
      ((and (= (length remaining-passport) 0) (> (length remaining-keys) 0)) false)
      ((and (= (length remaining-passport) 0) (= (length remaining-keys) 0)) true)
      (else (iter (remove (first (car remaining-passport)) remaining-keys) (cdr remaining-passport)))
    )
  )

  (iter required-keys passport)
)

(length (filter valid-passport passports))
