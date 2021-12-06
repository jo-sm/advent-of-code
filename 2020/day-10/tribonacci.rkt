#lang racket/base

(require "../utils.rkt")

; https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers#Tribonacci_numbers
(define (tribonacci n)
  (define a+ (expt (+ 19 (* 3 (sqrt 33))) (/ 1 3)))
  (define a- (expt (- 19 (* 3 (sqrt 33))) (/ 1 3)))
  (define b (expt (+ 586 (* 102 (sqrt 33))) (/ 1 3)))

  (inexact->exact (round (* 3 b (/ (expt (* (/ 1 3) (+ a+ a- 1)) n) (- (expt b 2) (* 2 b) -4))))))

(define (split-into-contiguous-regions lst [result '()])
  (cond
    [(null? lst) result]
    [(null? result) (split-into-contiguous-regions (cdr lst) (list (list (car lst))))]
    [(= (- (car lst) (caar result)) 1)
     (split-into-contiguous-regions (cdr lst) (cons (cons (car lst) (car result)) (cdr result)))]
    [else (split-into-contiguous-regions (cdr lst) (cons (list (car lst)) result))]))

(define example (cons 0 (sort (read-input-lines "example" #:line-parser string->number) <)))
(define input (cons 0 (sort (read-input-lines #:line-parser string->number) <)))

(apply *
       (map (lambda (i)
              (if (= (length i) 1)
                1
                (tribonacci (length i))))
            (split-into-contiguous-regions example)))
(apply *
       (map (lambda (i)
              (if (= (length i) 1)
                1
                (tribonacci (length i))))
            (split-into-contiguous-regions input)))

; (check-eq?
;   (count-possible-adapter-configurations (read-input-lines "example" #:line-parser string->number))
;   19208)

; (count-possible-adapter-configurations (read-input-lines #:line-parser string->number))
