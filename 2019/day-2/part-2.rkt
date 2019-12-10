#lang racket

#|
  Almost identical to part 1 (copy-pasted), except that a "program" can be run with arbitrary starting conditions
  in positions 1 and 2. Attempts to determine a specific output given starting conditions by doing the following:

  if generated output is equal to expected, end program and output starting conditions
  if generated output is less than expected, increment starting conditions 1 and 2
  if generated output is greater than expected:
    - decrement starting position 1 by conversion factor
    - decrement starting position 2 by (conversion factor + 1)
    - increment conversion factor by one

  This is likely a poor implementation of a search algorithm, but it gets the job done.
|#

(require "../utils.rkt")
(require "../Intcode/runner.rkt")

(define program-raw (flatten (read "Intcode.program" (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(define (run-program a b)
  (define program (list-set (list-set program-raw 1 a) 2 b))

  (first (second (run-intcode-program program)))
)

; Run a given program
;
; a        = starting position 1
; b        = starting position 2
; expected = expected output
; conv     = "conversion factor", see top
(define (run a b expected conv)
  (define output (run-program a b))

  (cond
    ((equal? output expected) (+ (* 100 a) b))
    ((< output expected) (run (+ a 1) (+ b 1) expected conv))
    ((> output expected) (run (- a conv) (- b (+ conv 1)) expected (+ conv 1)))
  )
)

(run 0 0 19690720 0)
