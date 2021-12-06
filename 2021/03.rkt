#lang racket/base

(require "../utils.rkt"
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
  (car (sort lists
             (λ (a b)
               (> (length a) (length b))))))

(define (invert-bit bit)
  (if (char=? bit #\0)
    #\1
    #\0))

; Given a list of bitlists, find the most common bit in all lists at a given position.
(define (get-most-common-bit bitlists i)
  (let-values ([(ones zeros) (~>> bitlists
                                  (map (λ (num)
                                         (list-ref num i)))
                                  ; #\1 is important as we want to prefer 1 if both lists are equal
                                  (partition (λ (char)
                                               (char=? char #\1))))])
    (car (longest ones zeros))))

(define (get-least-common-bit bitlists i)
  (invert-bit (get-most-common-bit bitlists i)))

; Translates a list of #\0 and #\1 chars into a number
(define (bitlist->number bitlist)
  (~> bitlist
      list->string
      (string->number 2)))

(define (part-1 input)
  (define size (length (car input)))
  (define most-common-bits
    (~>> size
         range
         (map (cut get-most-common-bit input <>))))
  (define least-common-bits (map invert-bit most-common-bits))

  (* (bitlist->number most-common-bits) (bitlist->number least-common-bits)))

(define (part-2 input)
  (define size (length (car input)))
  (define (get-rating bit-fn)
    (for/fold ([remaining input] #:result (car remaining))
      ([i (range size)] #:break (= (length remaining) 1))
      (define cur-bit (bit-fn remaining i))

      (filter (λ (num)
                (char=? (list-ref num i) cur-bit))
              remaining)))

  (define oxygen-gen-rating (get-rating get-most-common-bit))
  (define co2-scrubber-rating (get-rating get-least-common-bit))

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
(define input (parse "03.rktd" #t #:parser string->list))

(check-eq? (part-1 example) 198)
(check-eq? (part-1 input) 2498354)

(check-eq? (part-2 example) 230)
(check-eq? (part-2 input) 3277956)
