#lang racket/base

(require "../utils.rkt"
         threading
         racket/string
         racket/function
         racket/list
         rackunit)

#|
Another tricky day, this time caused by a mix of being hungover and using the wrong data structure. Needing to use
`caar`, `caadr`, etc. a lot makes it hard to understand what's going on and I spent a lot of time just getting that
to work (renaming them to x1, y1, etc. makes it a bit nicer but it's still not ideal). Using a struct for coords (or
at least a complex number) would have simplified things, and so the learning here is use the right data structure. I
avoided making a struct or complex number because I didn't want to complicate my parser (which already has a couple
nested maps) but it was a bad decision in hindsight.

Another complication is the fact that this solution is really slow, taking around 1 minute to run parts 1 and 2 on
the example and input. It's likely due to using `flatten` on such a large list. Eliminating `flatten` by folding rather
than mapping over the input and using `cons` may improve speed.

Update: removing flatten in favor of `foldl` significantly improves performance. Another alternative to explore could be
to use sets.
|#

(define x1 caar)
(define y1 caadr)
(define x2 cadar)
(define y2 cadadr)

(define x car)
(define y cadr)

(define (pair->line pair)
  (define start (car pair))
  (define end (cadr pair))

  (let loop ([result (list start)])
    (define last (car result))
    (define x-op
      (cond
        [(> (x last) (x end)) sub1]
        [(< (x last) (x end)) add1]
        [else identity]))
    (define y-op
      (cond
        [(> (y last) (y end)) sub1]
        [(< (y last) (y end)) add1]
        [else identity]))
    (define new-last (list (x-op (x last)) (y-op (y last))))

    (if (equal? new-last end)
      (reverse (cons new-last result))
      (loop (cons new-last result)))))

(define (part-1 input)
  (~>> input
       (filter (λ (pair)
                 (or (= (x1 pair) (y1 pair))
                     (= (x2 pair) (y2 pair)))))
       (foldl (λ (pair acc)
                (append (pair->line pair) acc))
              '())
       (group-by identity)
       (filter (λ (x)
                 (> (length x) 1)))
       (map car)
       length))

(define (part-2 input)
  (~>> input
       (foldl (λ (pair acc)
                (append (pair->line pair) acc))
              '())
       (group-by identity)
       (filter (λ (x)
                 (> (length x) 1)))
       (map car)
       length))

(define (parser line)
  (~> line
      (string-split " -> ")
      (map (λ (coord)
             (~> coord
                 (string-split ",")
                 (map string->number _)))
           _)))

(define example
  (map parser
       (list "0,9 -> 5,9"
             "8,0 -> 0,8"
             "9,4 -> 3,4"
             "2,2 -> 2,1"
             "7,0 -> 7,4"
             "6,4 -> 2,0"
             "0,9 -> 2,9"
             "3,4 -> 1,4"
             "0,0 -> 8,8"
             "5,5 -> 8,2")))
(define input (parse "05.rktd" #t #:parser parser))

(check-eq? (part-1 example) 5)
(check-eq? (part-1 input) 5197)

(check-eq? (part-2 example) 12)
(check-eq? (part-2 input) 18605)
