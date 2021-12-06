#lang racket

(require "../utils.rkt")
(require srfi/26)

(define (line-to-bags line)
  (define-values (_ bag-type r) (apply values (regexp-match #px"^([a-z ]+) bags contain (.*)" line)))

  (define inner-bag-types
    (filter (cut first <>)
            (map (compose rest (cut regexp-match #px"^([0-9]{1,2}) ([a-z ]+) bag|bags" <>))
                 (string-split r ", "))))

  (list bag-type inner-bag-types))

(define (could-contain-bag-type bag-type bags)
  (define (iter bag-type bags)
    (define next-bag-types
      (second (findf (lambda (bag) (equal? (first bag) (second bag-type)))
                bags)))

    (if (length next-bag-types)
      (list (first bag-type) (map (lambda (nbt) (iter nbt bags)) next-bag-types))
      (first bag-type)))

  (define (iter2 bag-declaration)
    (define num-this-bag (string->number (first bag-declaration)))
    (define bags-inside (second bag-declaration))

    (if (null? bags-inside)
      num-this-bag
      (+ num-this-bag (* num-this-bag (apply + (map iter2 bags-inside))))))

  (- (iter2 (iter bag-type bags)) 1))

(could-contain-bag-type (list "1" "shiny gold")
                        (read-input-lines "example" #:line-parser line-to-bags))
