#lang racket/base

(require "../utils.rkt")
(require racket/list)
(require rackunit)
(require srfi/26)

(define (mapmap fn lst lst2)
  (map (lambda (i) (map (lambda (j) (fn i j)) lst2)) lst))

(define (find-next-seat seats row col cur-row cur-col)
  (define possible-next-seat (list-ref-safe (list-ref-safe seats cur-row) cur-col))
  (define possible-next-cur-row (if (= row cur-row) row ((if (> cur-row row) + -) cur-row 1)))
  (define possible-next-cur-col (if (= col cur-col) col ((if (> cur-col col) + -) cur-col 1)))

  (cond
    [(and (= row cur-row) (= col cur-col)) null]
    [(null? possible-next-seat) null]
    [(not (equal? possible-next-seat #\.)) possible-next-seat]
    [else (find-next-seat seats row col possible-next-cur-row possible-next-cur-col)]))

(define (get-adjacent-seats seats row col)
  (define adjacent-seats
    (mapmap (lambda (row-num col-num) (find-next-seat seats row col row-num col-num))
            (range (- row 1) (+ row 2))
            (range (- col 1) (+ col 2))))

  (flatten adjacent-seats))

(define (new-seat-arrangement seats row col)
  (define seat (list-ref (list-ref seats row) col))
  (define adjacent-seats (get-adjacent-seats seats row col))

  (cond
    [(equal? seat #\.) #\.]
    [(and (equal? seat #\L) (not (findf (lambda (s) (equal? s #\#)) adjacent-seats))) #\#]
    [(and (equal? seat #\#) (>= (length (filter (lambda (s) (equal? s #\#)) adjacent-seats)) 5)) #\L]
    [else seat]))

(define (find-equilibrium seats [n 0])
  (define new-seats-arrangement
    (mapmap (cut new-seat-arrangement seats <> <>)
            (range 0 (length seats))
            (range 0 (length (first seats)))))

  (if (not (equal? seats new-seats-arrangement))
      (find-equilibrium new-seats-arrangement (+ n 1))
      (values new-seats-arrangement n)))

(define example (read-input-lines "example" #:line-parser string->list))
(define input (read-input-lines #:line-parser string->list))

(define-values (example-arrangement example-iteration-count) (find-equilibrium example))

(check-eq? (count (lambda (i) (equal? i #\#)) (flatten example-arrangement)) 26)
example-iteration-count

(define-values (input-arrangement input-iteration-count) (find-equilibrium input))

(count (lambda (i) (equal? i #\#)) (flatten input-arrangement))
input-iteration-count
