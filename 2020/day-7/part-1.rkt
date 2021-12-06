#lang racket

(require "../utils.rkt")
(require srfi/26)

(define (line-to-bags line)
  (define-values (_ bag-type r) (apply values (regexp-match #px"^([a-z ]+) bags contain (.*)" line)))

  (define inner-bag-types
    (map (compose rest (cut regexp-match #px"^([0-9]{1,2}) ([a-z ]+) bag|bags" <>))
         (string-split r ", ")))

  (list bag-type inner-bag-types))

(define (could-contain-bag-type bag-type bags)
  (define (iter bag-type bags)
    (define a
      (map first
           (filter (lambda (bag)
                     (findf (lambda (g) (equal? (second g) bag-type))
                       (second bag)))
                   bags)))

    (if (length a)
      (append a (map (cut iter <> bags) a))
      '()))

  (list->set (flatten (iter bag-type bags))))

(set-count (could-contain-bag-type "shiny gold" (read-input-lines #:line-parser line-to-bags)))
