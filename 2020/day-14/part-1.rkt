#lang racket/base

(require "../utils.rkt")
(require racket/string)
(require racket/list)
(require rackunit)

(struct instruction (op arguments) #:transparent)
(struct bitmask (num loc) #:transparent)

(define (char->number char)
  (string->number (string char)))

(define (translate-mask mask [n 0] [result '()])
  (cond
    [(= n (string-length mask)) result]

    ; Create a bitmask instance if the character is a number
    [(char-numeric? (string-ref mask n))
     (translate-mask
      mask
      (+ n 1)
      ; Order doesn't matter so we can just cons
      (cons (bitmask (char->number (string-ref mask n)) (- (string-length mask) n 1)) result))]
    [else (translate-mask mask (+ n 1) result)]))

(define (line->instruction line)
  (define-values (raw-operation argument) (apply values (string-split line " = ")))

  (cond
    [(equal? raw-operation "mask") (instruction "mask" (translate-mask argument))]
    [(regexp-match? "mem" raw-operation)
     (instruction "mem"
                  (list (string->number (car (regexp-match #px"[0-9]+" raw-operation)))
                        (string->number argument)))]))

(define (apply-memory-instruction memory instruction mask)
  (define memory-address (first (instruction-arguments instruction)))
  (define new-unmasked (second (instruction-arguments instruction)))

  (define (bm-fold bm result)
    (cond
      ; Subtract 2^(bitmask location) from the result if the bit is set and it shouldn't be
      [(and (bitwise-bit-set? result (bitmask-loc bm)) (= (bitmask-num bm) 0))
       (- result (expt 2 (bitmask-loc bm)))]

      ; Add 2^(bitmask location) to the result if the bit is not set and it should be
      [(and (not (bitwise-bit-set? result (bitmask-loc bm))) (= (bitmask-num bm) 1))
       (+ result (expt 2 (bitmask-loc bm)))]

      [else result]))

  (hash-set memory memory-address (foldl bm-fold new-unmasked mask)))

(define (run-instructions instructions)
  (define (iter remaining mask memory)
    (cond
      [(null? remaining) memory]

      ; Set the new mask
      [(equal? (instruction-op (car remaining)) "mask")
       (iter (cdr remaining) (instruction-arguments (car remaining)) memory)]

      ; Apply this memory instruction with the current mask
      [(equal? (instruction-op (car remaining)) "mem")
       (iter (cdr remaining) mask (apply-memory-instruction memory (car remaining) mask))]))

  (iter instructions '() (make-immutable-hash)))

(check-eq? (apply +
                  (hash-values (run-instructions
                                (read-input-lines "example" #:line-parser line->instruction))))
           165)

(check-eq?
 (apply + (hash-values (run-instructions (read-input-lines #:line-parser line->instruction))))
 10717676595607)
