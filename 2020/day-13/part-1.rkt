#lang racket/base

(require "../utils.rkt")
(require rackunit)
(require racket/string)

(define (file-parser file)
  (define parts (string-split file))
  (define earliest-departure (string->number (car parts)))
  (define possible-bus-numbers
    (sort (filter number? (map string->number (string-split (cadr parts) ","))) <))

  (list earliest-departure possible-bus-numbers))

(define (find-bus-with-wait notes)
  (define earliest-departure (car notes))
  (define bus-numbers (cadr notes))
  (define later-bus-numbers
    (map (lambda (n) (* n (+ 1 (quotient earliest-departure n)))) bus-numbers))

  (define first-possible-bus-number (car (sort later-bus-numbers <)))
  (define original-bus-number
    (findf (lambda (n) (= (remainder first-possible-bus-number n) 0))
      bus-numbers))

  (list original-bus-number (- first-possible-bus-number earliest-departure)))

(check-eq? (apply * (find-bus-with-wait (read-input-file "example" #:file-parser file-parser))) 295)

(apply * (find-bus-with-wait (read-input-file #:file-parser file-parser)))
