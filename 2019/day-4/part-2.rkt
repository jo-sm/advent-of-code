#lang racket

(define password-range (range 128392 643281))

; Takes a character that represents a digit and transforms it into a number
; E.g. (char->number #\1) -> 1
;
; The link to the SO answer that gave me this idea is lost, but the idea is
; that #\0 is 48 in ASCII, and so subtracting that from the converted given
; character will result in the actual value.
(define (char->number c)
  (- (char->integer c) (char->integer #\0))
)

; Removes values that are the same as `val` at the
; beginning of `lst`
(define (remove-initial-duplicates val lst)
  (define (iter val result)
    (cond
      ((null? result) result)
      ((= val (car result)) (iter (cdr result) val))
      (else result)
    )
  )

  (iter val lst)
)

; Returns true if the values in the given `password` always increase, where
; "increase" is defined as being greater than or equal.
(define (always-increasing password)
  (define (iter result digit remaining-password)
    (cond
      ((null? remaining-password) result)
      ((>= (car remaining-password) digit) (iter true (car remaining-password) (cdr remaining-password)))

      ; If the next value is less than the current, then end at the next iteration
      (else (iter false 0 null))
    )
  )

  (iter true (car password) (cdr password))
)

; Returns true if there are any groups of two of any digit
; in the given password.
;
; (has-solo-double 123345) -> true
; (has-solo-double 123334) -> false
(define (has-group-of-two password)
  (define (iter result digit remaining-password)
    (cond
      ((null? remaining-password) result)

      ; If the length of `remaining-password` with `digit` removed from the front is equal
      ; to the length of the next iteration's `remaining-password`, then we know we have a
      ; duplicated `digit` that is in a group of two.
      ((= (length (remove-initial-duplicates digit remaining-password)) (length (cdr remaining-password))) (iter true 0 null))

      ; Iterate with the next digit and removing any initial duplicated digits from the next `remaining-password`.
      ;
      ; This is necessary because the above condition does not know if there is no group or a group
      ; larger than two, and because we do not need to know the length of a given group, this is safe.
      (else (iter false (car remaining-password) (remove-initial-duplicates digit (cdr remaining-password))))
    )
  )

  (iter false (car password) (cdr password))
)

(define (is-valid-password i)
  ; Transform the given password into a list of digits, by converting from number to string, the string to a list,
  ; then the character to a digit
  (define digits (map char->number (string->list (number->string i))))

  (and (always-increasing digits) (has-group-of-two digits))
)

(length (filter is-valid-password password-range))
