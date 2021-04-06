#lang racket

#|
  Another big learning today: the concent of Chinese Remainder Theorem.

  I spent *hours* on this problem, attempting to find if there was some way to
  reduce the complexity of the problem. I thought for a moment that it might be
  memoization, because of the wording used in the puzzle statement, but that was
  quickly ruled out since it doesn't have a recursive definition.

  After finding a brute force method (finding a number `n`, starting from 1, that
  all numbers minus their "position" are cleanly divided by), and testing on some of
  the examples, it became clear that a more clever approach would be necessary. First
  I thought I could find a clever solution that looked like this:

  ; given 17,x,13,19
  (+ (* n 17) (* (+ n 2) 13) (* (+ n 3) 19))

  but found that it didn't work. I then tried to turn the problem into a series of linear
  equations:

  17a +  0b +  0c + k = 0
   0a + 13b +  0c + k = 2
   0a +  0b + 19c + k = 3

  And while it seemed somewhat promising I couldn't find the right way to approach it. I
  plugged in some reduced form of the equations into Wolfram Alpha and did get an "integer solution"
  but didn't know how to generally compute that. I then looked at the subreddit and saw Chinese
  Remainder Theorem, and after a lot of searching, found some good, easy to digest resources that
  explained it in a way that I could translate into code. The last remaining piece was finding that
  the remainder is not the expected position (e.g. 2 for 13, above), but rather the difference
  between the expected position and the "bus number" (e.g. 13 - 2 = 11), since we want a number
  that is "expected position" short of a number where the modulo of n is 0, e.g. (modulo 26 13).
|#

(require "../utils.rkt")
(require rackunit)
(require math/number-theory)
(require srfi/26)

(define (file-parser file)
  (define parts (string-split file))
  (define schedule (string-split (cadr parts) ","))
  (define bus-numbers (filter number? (map string->number schedule)))

  (define expected-times (map (lambda (n) (index-of schedule (number->string n))) bus-numbers))

  (map list bus-numbers expected-times))

(define (find-first-possible-time buses-with-pos)
  (define m (foldl (lambda (i memo) (* (car i) memo)) 1 buses-with-pos))

  (define (find-result cur)
    (define m-i (/ m (car cur)))
    (define x-i (modular-inverse m-i (car cur)))

    (* m-i x-i (- (car cur) (cadr cur))))

  (modulo (apply + (map find-result buses-with-pos)) m))

(check-eq? (find-first-possible-time (read-input-file "example" #:file-parser file-parser)) 1068781)

(find-first-possible-time (read-input-file #:file-parser file-parser))
