#lang racket/base

#|
  Spent over an hour wondering why my way of doing this wasn't working, until I re-read the instructions
  and realized I missed one difference between part 1 and 2 🤦‍♂️
|#

(require "../utils.rkt")
(require racket/string)
(require racket/list)
(require rackunit)
(require srfi/26)

(struct instruction (op arguments) #:transparent)
(struct bitmask (num loc) #:transparent)

(define (char->number char)
  (string->number (string char)))

(define (translate-mask mask [n 0] [result (list)])
  (cond
    [(= n (string-length mask)) result]
    [(not (equal? (string-ref mask n) #\X))
     (translate-mask
      mask
      (+ n 1)
      (cons (bitmask (char->number (string-ref mask n)) (- (string-length mask) n 1)) result))]
    [else (translate-mask mask (+ n 1) (cons (bitmask 2 (- (string-length mask) n 1)) result))]))

(define (line->instruction line)
  (define pieces (string-split line " = "))

  (cond
    [(equal? (car pieces) "mask") (instruction "mask" (translate-mask (cadr pieces)))]
    [(regexp-match? "mem" (car pieces))
     (instruction "mem"
                  (list (string->number (car (regexp-match #px"[0-9]+" (car pieces))))
                        (string->number (second pieces))))]))

(define (apply-memory-instruction memory instruction mask)
  (define memory-address (first (instruction-arguments instruction)))
  (define new-value (second (instruction-arguments instruction)))

  (define (apply-x-mask address pos)
    (if (bitwise-bit-set? address pos)
        (list address (- address (expt 2 pos)))
        (list address (+ address (expt 2 pos)))))

  (define (iter remaining-mask addresses)
    (cond
      [(null? remaining-mask) addresses]
      [(not (= (bitmask-num (car remaining-mask)) 2)) (iter (cdr remaining-mask) addresses)]
      [else (iter (cdr remaining-mask)
                  (flatten (map (cut apply-x-mask <> (bitmask-loc (car remaining-mask)))
                                addresses)))]))

  (define (bm-fold bm result)
    (cond
      [(and (bitwise-bit-set? result (bitmask-loc bm)) (= (bitmask-num bm) 2))
       (- result (expt 2 (bitmask-loc bm)))]
      [(and (not (bitwise-bit-set? result (bitmask-loc bm))) (= (bitmask-num bm) 1))
       (+ result (expt 2 (bitmask-loc bm)))]
      [else result]))

  (define semi-parsed-memory-address (foldl bm-fold memory-address mask))

  (define r (iter mask (list semi-parsed-memory-address)))

  (map (cut hash-set! memory <> new-value) r)

  memory)

(define (run-instructions instructions)
  (define (iter remaining mask memory)
    (cond
      [(null? remaining) memory]
      [(equal? (instruction-op (car remaining)) "mask")
       (iter (cdr remaining) (instruction-arguments (car remaining)) memory)]
      [(equal? (instruction-op (car remaining)) "mem")
       (iter (cdr remaining) mask (apply-memory-instruction memory (car remaining) mask))]))

  (iter instructions "" (make-hash)))

(check-eq? (apply +
                  (hash-values (run-instructions
                                (read-input-lines "example2" #:line-parser line->instruction))))
           208)

(check-eq?
 (apply + (hash-values (run-instructions (read-input-lines #:line-parser line->instruction))))
 3974538275659)
