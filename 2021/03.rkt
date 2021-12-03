#lang racket/base

(require "./utils.rkt"
         threading
         racket/list
         srfi/26
         rackunit)

#|
Day 3: Binary Diagnostic

Slightly more challenging today, but not too bad. Part 1 was pretty straightforward, and thanks
to the way I set it up part 2 was relatively simple to do as well. The most challenging part was
honestly just understanding part 2 as it was really wordy.
|#

; Returns the longest provided list. If two lists are of equal length and would be
; considered the longest the first is returned.
(define (longest . lists)
  (for/fold ([result '()]) ([lst lists])
    (if (> (length lst) (length result)) lst result)))

; Given a list of bitlists, find the most common bit in all lists at a given position.
(define (get-most-common-bit bitlists i)
  (let-values ([(ones zeros) (~>> bitlists
                                  (map (λ (num) (list-ref num i)))
                                  ; #\1 is important as we want to prefer 1 if both lists are equal
                                  (partition (λ (char) (char=? char #\1))))])
    (car (longest ones zeros))))

; Opposite of `get-most-common-bit`.
(define (get-least-common-bit bitlists i)
  (let ([bit (get-most-common-bit bitlists i)]) (if (char=? bit #\0) #\1 #\0)))

; Translates a list of #\0 and #\1 chars into a number
(define (bitlist->number bitlist)
  (~> bitlist list->string (string->number 2)))

(define (part-1 input)
  (define size (length (car input)))
  (define most-common-bits (~>> size range (map (cut get-most-common-bit input <>))))
  (define least-common-bits (~>> size range (map (cut get-least-common-bit input <>))))

  (* (bitlist->number most-common-bits) (bitlist->number least-common-bits)))

(define (part-2 input)
  (define size (length (car input)))
  (define oxygen-gen-rating
    (for/fold ([remaining input] #:result (car remaining))
              ([i (range size)] #:break (= (length remaining) 1))
      (define cur-most-common-bit (get-most-common-bit remaining i))

      (filter (λ (num) (char=? (list-ref num i) cur-most-common-bit)) remaining)))

  (define co2-scrubber-rating
    (for/fold ([remaining input] #:result (car remaining))
              ([i (range size)] #:break (= (length remaining) 1))
      (define cur-least-common-bit (get-least-common-bit remaining i))

      (filter (λ (num) (char=? (list-ref num i) cur-least-common-bit)) remaining)))

  (* (bitlist->number oxygen-gen-rating) (bitlist->number co2-scrubber-rating)))

(define example
  ; TODO this formatting sucks. Maybe it's related to https://github.com/sorawee/fmt/issues/24
  (map string->list
       '("00100" "11110"
                 "10110"
                 "10111"
                 "10101"
                 "01111"
                 "00111"
                 "11100"
                 "10000"
                 "11001"
                 "00010"
                 "01010")))
(define input (read-input-lines "03.rktd" #:line-parser string->list))

(check-eq? (part-1 example) 198)
(check-eq? (part-1 input) 2498354)

(check-eq? (part-2 example) 230)
(check-eq? (part-2 input) 3277956)
