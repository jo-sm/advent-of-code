#lang racket/base

(require rackunit)
(require racket/list)
(require srfi/26)
(require "../utils.rkt")

(define (find-joltage-differences raw-joltages)
  (define adapter-joltages (sort raw-joltages <))
  (define all-joltages (append (cons 0 adapter-joltages) (list (+ (last adapter-joltages) 3))))

  (define (generate-joltage-difference-list differences remaining-joltages)
    (if (= (length remaining-joltages) 1)
        differences
        (generate-joltage-difference-list
         (append differences (list (- (cadr remaining-joltages) (car remaining-joltages))))
         (cdr remaining-joltages))))

  (generate-joltage-difference-list '() all-joltages))

(define (magic-joltage-number differences)
  (* (count (cut = <> 1) differences) (count (cut = <> 3) differences)))

(check-eq? (magic-joltage-number (find-joltage-differences
                                  (read-input-lines "example" #:line-parser string->number)))
           220)

(magic-joltage-number (find-joltage-differences (read-input-lines #:line-parser string->number)))
