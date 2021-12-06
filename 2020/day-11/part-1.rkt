#lang racket/base

(require "../utils.rkt")
(require racket/list)
(require rackunit)

(define (list-ref-safe lst i)
  (if (or (>= i (length lst))
          (< i 0))
    null
    (list-ref lst i)))

(define (mapmap fn lst lst2)
  (map (lambda (i) (map (lambda (j) (fn i j)) lst2)) lst))

(define (get-adjacent-seats seats row col)
  (define adjacent-seats
    (mapmap (lambda (row-num col-num)
              (if (and (= col-num col)
                       (= row-num row))
                null
                (list-ref-safe (list-ref-safe seats row-num) col-num)))
            (range (- row 1) (+ row 2))
            (range (- col 1) (+ col 2))))

  (flatten adjacent-seats))

(define (new-seat-arrangement seats row col)
  (define seat (list-ref (list-ref seats row) col))
  (define adjacent-seats (get-adjacent-seats seats row col))

  (cond
    [(equal? seat #\.) #\.]
    [(and (equal? seat #\L)
          (not (findf (lambda (s) (equal? s #\#))
                 adjacent-seats)))
     #\#]
    [(and (equal? seat #\#)
          (>= (length (filter (lambda (s) (equal? s #\#)) adjacent-seats)) 4))
     #\L]
    [else seat]))

(define (find-equilibrium seats)
  (define new-seats-arrangement
    (map (lambda (row-num)
           (map (lambda (col-num) (new-seat-arrangement seats row-num col-num))
                (range 0 (length (first seats)))))
         (range 0 (length seats))))

  (if (not (equal? seats new-seats-arrangement))
    (find-equilibrium new-seats-arrangement)
    new-seats-arrangement))

(check-eq? (count (lambda (i) (equal? i #\#))
                  (flatten (find-equilibrium
                            (read-input-lines "example" #:line-parser string->list))))
           37)

(count (lambda (i) (equal? i #\#))
       (flatten (find-equilibrium (read-input-lines #:line-parser string->list))))
