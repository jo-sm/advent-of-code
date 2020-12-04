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

(struct height (length unit))

(define (item-to-kv item)
  (define-values (key raw-value) (list->values (string-split item ":")))
  (define value (case key
    (("byr" "iyr" "eyr") (let ([n (string->number raw-value)])
      (if n n 0)
    ))
    (("hgt") (let ([parsed (regexp-match #px"^([0-9]{2,3})(cm|in)$" raw-value)])
      (if parsed
        (height (string->number (second parsed)) (third parsed))
        (height 0 "cm")
      )
    ))
    (else raw-value)
  ))

  (list key value)
)

(define (validate-kv key value)
  (case key
    (("byr") (and (>= value 1920) (<= value 2002)))
    (("iyr") (and (>= value 2010) (<= value 2020)))
    (("eyr") (and (>= value 2020) (<= value 2030)))
    (("hgt") (if (equal? (height-unit value) "cm")
      (and (>= (height-length value) 150) (<= (height-length value) 193))
      (and (>= (height-length value) 59) (<= (height-length value) 76))
    ))
    (("hcl") (regexp-match? #px"^#[0-9a-f]{6}$" value))
    (("ecl") (number? (index-of (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth") value)))
    (("pid") (regexp-match? #px"^[0-9]{9}$" value))
    (("cid") true)
    (else false)
  )
)

(define (is-valid-passport passport)
  (define required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

  (define (iter rem-keys rem-passport)
    (cond
      ; There should be no keys remaining
      ((= (length rem-passport) 0) (= (length rem-keys) 0))

      ; If the kv is valid, remove that key and continue with rest of passport
      ((apply validate-kv (car rem-passport)) (iter (remove (first (car rem-passport)) rem-keys) (cdr rem-passport)))

      ; kv is not valid
      (else false)
    )
  )

  (iter required-keys passport)
)

(define passports
  (map
    (lambda (item) (map item-to-kv item))
    (map
      string-split
      (read-input-file #:file-parser (lambda (f) (string-split f "\n\n")))
    )
  )
)

(length (filter is-valid-passport passports))
