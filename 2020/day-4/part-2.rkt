#lang racket

; Much trickier than it should have been...
;
; The initial implementation of the validators was pretty straightforward, although they're not
; as clean as I would like. However I initially got an answer that was too high, which meant that
; one or more of my validators was not strict enough.
;
; I looked for a while at each of the validators, and was able to find one issue with my height
; validator (I didn't check that "cm" or "in" was actually in the string). However I got stuck
; after this, and after a lot of thinking and walking away for a few mins I tested the regex for
; the pid validation, and saw that it was too loose (1756319674 was considered valid, which is
; too long), finally noticing I should have put ^$ in my regex 🤦‍♂️

(require "../utils.rkt")

(define passports
  (map (lambda (x) (map (lambda (y) (string-split y ":")) x)) (map string-split (string-split (read-file "input") "\n\n")))
)

(define (validate-height value)
  (define num-cm (string->number (first (string-split value "cm"))))
  (define num-in (string->number (first (string-split value "in"))))

  (cond
    (num-cm (and (string-suffix? value "cm") (>= num-cm 150) (<= num-cm 193)))
    (num-in (and (string-suffix? value "in") (>= num-in 59) (<= num-in 76)))
    (else false)
  )
)

(define (validate-year value min max)
  (define value-year (string->number value))

  (if value-year
    (and (>= value-year min) (<= value-year max))
    false
  )
)

(define (validate-key key value)
  (case key
    (("byr") (validate-year value 1920 2002))
    (("iyr") (validate-year value 2010 2020))
    (("eyr") (validate-year value 2020 2030))
    (("hgt") (validate-height value))
    (("hcl") (regexp-match? #px"^#[0-9a-f]{6}$" value))
    (("ecl") (index-of (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth") value))
    (("pid") (regexp-match? #px"^[0-9]{9}$" value))
    (("cid") true)
    (else false)
  )
)

(define (valid-passport passport)
  (define required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

  (define (iter remaining-keys remaining-passport)
    (cond
      ((= (length remaining-passport) 0) (= (length remaining-keys) 0))
      ((apply validate-key (car remaining-passport)) (iter (remove (first (car remaining-passport)) remaining-keys) (cdr remaining-passport)))
      (else false)
    )
  )

  (iter required-keys passport)
)

(length (filter valid-passport passports))
