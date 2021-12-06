#lang racket/base

(require "../utils.rkt"
         racket/list
         rackunit
         threading)

#| Day 1: Sonar Sweep

Part one

This one, as expected, was pretty simple. The problem is related to being given a list of numbers
and finding the difference between them, and find the differences that are positive. The idea that
came to mind initially was to reduce the list from the original numbers into a list of the diffs,
then take the positive ones, and count how many there were. This turned out to be a good approach as
it got the right answer on the first try.
|#

(define (part-1 nums)
  (define differences
    (for/fold ([diffs '()] [prev-num +nan.0] #:result diffs)
      ([num nums])
      (if (empty? diffs)
        (values '(0) num)
        (values (cons (- num prev-num) diffs) num))))
  (~>> differences
       (filter positive?)
       length))

#|
Mostly the same as part 1, except this time instead of taking the difference between two numbers,
we need to take the difference between the sums of a rolling set of 3 numbers. Thanks for the original
approach of generating the differences it required just a little bit of modification. Initially I
through I might want to just keep the last three numbers in a list, but realized that it's simpler to
just keep all the numbers in a list as they appear and just call `take` on it.

Although it was simple to modify I don't really like the code below because of the nested use of cons, so
I want to find a better and more elegant approach using the same method.
|#

(define (part-2 nums)
  (define differences
    (for/fold ([diffs '()] [prev-nums '()] #:result diffs)
      ([num nums])
      (if (< (length prev-nums) 3)
        (values diffs (cons num prev-nums))
        (values (cons (- (apply + (cons num (take prev-nums 2))) (apply + (take prev-nums 3))) diffs)
                (cons num prev-nums)))))
  (~>> differences
       (filter positive?)
       length))

(define parsed (parse "01.rktd" #t #:parser string->number))
(define example '(199 200 208 210 200 207 240 269 260 263))

(check-eq? (part-1 example) 7)
(check-eq? (part-1 parsed) 1696)

(check-eq? (part-2 example) 5)
(check-eq? (part-2 parsed) 1737)
