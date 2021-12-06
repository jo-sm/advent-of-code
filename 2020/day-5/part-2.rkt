#lang racket

(require "../utils.rkt")

(struct seat (row col))
(define (seat-id seat)
  (+ (* (seat-row seat) 8) (seat-col seat)))

(define (create-partition-operations line)
  (list (string->list (substring line 0 7)) (string->list (substring line 7))))

(define (partition operation lst)
  (if (index-of '(#\F #\L) operation)
    (take lst (/ (length lst) 2))
    (drop lst (/ (length lst) 2))))

(define (generate-seat-from-operations partition-operations)
  (seat (first (foldl partition (range 0 128) (first partition-operations)))
        (first (foldl partition (range 0 8) (second partition-operations)))))

(define (missing-seat-id seats)
  (define (find-missing-seat-id remaining-seats)
    (if (= (car remaining-seats) (+ (cadr remaining-seats) 1))
      (find-missing-seat-id (cdr remaining-seats))
      (- (car remaining-seats) 1)))

  (find-missing-seat-id seats))

(define seats
  (map generate-seat-from-operations (read-input-lines #:line-parser create-partition-operations)))

(define seat-ids (map seat-id seats))

(missing-seat-id (sort seat-ids >))
