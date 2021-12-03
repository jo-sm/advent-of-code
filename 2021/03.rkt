#lang racket/base

(require "./utils.rkt"
         threading
         racket/list
         srfi/26
         rackunit)

(define (longest . lists)
  (for/fold ([result '()]) ([lst lists])
    (if (> (length lst) (length result)) lst result)))

(define (get-most-common-bit nums i)
  (let-values ([(ones zeros) (~>> nums
                                  (map (λ (num) (string-ref num i)))
                                  ; #\1 is important as we want to prefer 1 if both lists are equal
                                  (partition (λ (char) (char=? char #\1))))])
    (car (longest ones zeros))))

(define (get-least-common-bit nums i)
  (let ([bit (get-most-common-bit nums i)]) (if (char=? bit #\0) #\1 #\0)))

(define (bitstring->number lst)
  (string->number lst 2))

(define (part-1 input)
  (define size (string-length (car input)))
  (define most-common-bits (~>> size range (map (cut get-most-common-bit input <>)) (apply string)))
  (define least-common-bits (~>> size range (map (cut get-least-common-bit input <>)) (apply string)))

  (* (bitstring->number most-common-bits) (bitstring->number least-common-bits)))

(define (part-2 input)
  (define size (string-length (car input)))
  (define oxygen-gen-rating
    (for/fold ([remaining input] #:result (car remaining))
              ([i (range size)] #:break (= (length remaining) 1))
      (define cur-most-common-bit (get-most-common-bit remaining i))

      (filter (λ (num) (char=? (string-ref num i) cur-most-common-bit)) remaining)))

  (define co2-scrubber-rating
    (for/fold ([remaining input] #:result (car remaining))
              ([i (range size)] #:break (= (length remaining) 1))
      (define cur-least-common-bit (get-least-common-bit remaining i))

      (filter (λ (num) (char=? (string-ref num i) cur-least-common-bit)) remaining)))

  (* (bitstring->number oxygen-gen-rating) (bitstring->number co2-scrubber-rating)))

(define example
  '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))
(define input (read-input-lines "03.rktd"))

(check-eq? (part-1 example) 198)
(check-eq? (part-1 input) 2498354)

(check-eq? (part-2 example) 230)
(check-eq? (part-2 input) 3277956)
