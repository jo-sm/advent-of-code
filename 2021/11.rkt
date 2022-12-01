#lang racket/base

(require "../utils.rkt"
         racket/list
         threading)

; nums 1 1 1 1 1 ...
; length 25
; size 5
; i 7
; y 1
; x 2

(define (map-with-index proc lst)
  (map (lambda (i) (proc (list-ref lst i) i)) (range (length lst))))

(define (foldl-with-index proc init lst)
  (foldl (lambda (i acc) (proc (list-ref lst i) i)) init (range (length lst))))

(define (get-adjacent nums i)
  (define size (sqrt (length nums))) ; assuming square
  (define x (remainder i size))
  (define y (floor (/ i size)))

  (define xs
    (cond
      [(= x 0) (list x 1)]
      [(= x (sub1 size)) (list x (- size 2))]
      [else (list (sub1 x) x (add1 x))]))

  (define ys
    (cond
      [(= y 0) (list y 1)]
      [(= y (sub1 size)) (list y (- size 2))]
      [else (list (sub1 y) y (add1 y))]))

  (~>> (cartesian-product xs ys)
       (filter (lambda (c) (not (equal? c (list x y)))))
       (map (lambda (c) (+ (* (cadr c) size) (car c))))
       (map (lambda (i) (list-ref nums i)))))

(define (identity n)
  n)

(define (increase-energy nums)
  (define increased
    (~>> nums
         (map identity)
         (map add1)))

  (values increased))

(define (increase-energy-all nums [indexes-to-increase (range (length nums))] [increased '()])
  (~>> nums
       (foldl (lambda (n) ()) '())))

(define (parser lines)
  (~>> lines
       (map string->list)
       (mapmap char->number)
       (apply append)))

(define example (parser '("11111" "19991" "19191" "19991" "11111")))
; (define example (parser '("12345" "09876" "54791" "09011" "68302")))

(increase-energy example)

; ; 0 2 9
; (get-adjacent example 0)
; ; 2 4 9 8 7
; (get-adjacent example 2)
; ; 9 8 7 6 4 9 9 0 1 1
; (get-adjacent example 12)
