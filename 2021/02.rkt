#lang racket/base

(require "../utils.rkt"
         rackunit
         threading
         racket/string
         racket/match)

#|
  Simple enough - for each line of instructions, modify the current `position` and `depth`
  of the sub based on the command. Forward increments the position, down increments the depth,
  and up decrements the depth, by the value given to the command.
|#

(define (part-1 instructions)
  (for/fold ([position 0] [depth 0] #:result (* position depth))
    ([i instructions])
    (match i
      [(cons "forward" p) (values (+ position p) depth)]
      [(cons "down" d) (values position (+ depth d))]
      [(cons "up" d) (values position (- depth d))])))

#|
  Almost the same as part one, except the depth is modified as a result of the current `aim` in the
  `forward` command rather than inc/dec'ing based on the down/up command.

  I got a little tripped up as I was inc/dec the depth in the down/up command, until I reread the
  example and realized that's not the case anymore.
|#
(define (part-2 instructions)
  (for/fold ([position 0] [depth 0] [aim 0] #:result (* position depth))
    ([i instructions])
    (match i
      [(cons "forward" u) (values (+ position u) (+ depth (* aim u)) aim)]
      [(cons "down" u) (values position depth (+ aim u))]
      [(cons "up" u) (values position depth (- aim u))])))

(define (parser line)
  (define split (string-split line))

  (cons (car split) (string->number (cadr split))))

(define example (map parser (list "forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2")))
(define input (parse "02.rktd" #t #:parser parser))

(check-eq? (part-1 example) 150)
(check-eq? (part-1 input) 1451208)

(check-eq? (part-2 example) 900)
(check-eq? (part-2 input) 1620141160)
