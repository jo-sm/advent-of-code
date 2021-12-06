#lang racket

(define (number->digits number)
  (define (iter digits remaining-number)
    (let-values ([(next-remaining-number digit) (quotient/remainder remaining-number 10)])
      (if (= next-remaining-number 0)
        (cons digit digits)
        (iter (cons digit digits) next-remaining-number))))

  (iter '() number))

; Removes values that are the same as `v` at the
; beginning of `lst`
(define (remove-initial-duplicates v lst)
  (define (iter v result)
    (cond
      [(null? result) result]
      [(= v (car result)) (iter (cdr result) v)]
      [else result]))

  (iter val lst))

; Returns true if the values in the given `password` always increases, where
; "increase" is defined as being greater than or equal.
(define (always-increasing password)
  (equal? (sort password <) password))

; Returns true if there are any groups of two of any digit
; in the given password.
;
; (has-solo-double 123345) -> true
; (has-solo-double 123334) -> false
(define (has-group-of-two password)
  (define (iter result digit remaining-password)
    (cond
      [(null? remaining-password) result]

      ; If the length of `remaining-password` with `digit` removed from the front is equal
      ; to the length of the next iteration's `remaining-password`, then we know we have a
      ; duplicated `digit` that is in a group of two.
      [(= (length (remove-initial-duplicates digit remaining-password))
          (length (cdr remaining-password)))
       (iter true 0 null)]

      ; Iterate with the next digit and removing any initial duplicated digits from the next `remaining-password`.
      ;
      ; This is necessary because the above condition does not know if there is no group or a group
      ; larger than two, and because we do not need to know the length of a given group, this is safe.
      [else (iter false
                  (car remaining-password)
                  (remove-initial-duplicates digit (cdr remaining-password)))]))

  (iter false (car password) (cdr password)))

(define (is-valid-password password)
  ; Transform the given password into a list of digits, by converting from number to string, the string to a list,
  ; then the character to a digit
  (define digits (number->digits password))

  (and (always-increasing digits)
       (has-group-of-two digits)))

(define possible-passwords (range 128392 643281))

(length (filter is-valid-password possible-passwords))
