#lang racket

; Pretty simple - the trickiest part was thinking about the best way to do the lambda
; that turns "byr:1992" into ("byr" "1992").

(require "../utils.rkt")
(require srfi/26)

(define (is-valid-passport passport)
  (define required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

  (define (passport-has-required-keys remaining-keys remaining-passport)
    (if (= (length remaining-passport) 0)
      ; If there's no more passport remaining, check to see if we've removed all the required keys
      (= (length remaining-keys) 0)

      ; Maybe remove the current key from the required keys and continue
      (passport-has-required-keys
        (remove (caar remaining-passport) remaining-keys)
        (cdr remaining-passport)))
  )

  (passport-has-required-keys required-keys passport)
)

; List of passport items, e.g. ("iyr:2010" "ecl:gry" ...)
(define raw-passports
  (map
    string-split
    (read-input-file #:file-parser (cut string-split <> "\n\n")))
)

; List of processed passports, e.g. (("iyr" "2010" ("ecl" "gry")))
(define passports
  ; Map over each "raw" passport, and split each of its keys by ":"
  (map
    (lambda (raw-passport)
      (map
        (lambda (passport-item) (string-split passport-item ":"))
        raw-passport))
    raw-passports
  )
)

(length (filter is-valid-passport passports))
