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

(define (read)
  (define io (open-input-file "Intcode.program" #:mode 'text))
  ; Assume that there is only one line for a given program
  (define raw (read-line io))

  (define program (map (lambda (i) (string->number i)) (string-split raw ",")))

  (close-input-port io)

  program
)

(define (replace-in-list lst pos val)
  (flatten (list (take lst pos) val (drop lst (+ pos 1))))
)

(define (process-opcode call program a b c)
  (define i (list-ref program a))
  (define j (list-ref program b))
  (define output (call i j))

  (replace-in-list program c output)
)

(define (run-instruction program pos)
  (define opcode (list-ref program pos))
  (define 1st (list-ref program (+ pos 1)))
  (define 2nd (list-ref program (+ pos 2)))
  (define 3rd (list-ref program (+ pos 3)))
  (define call (cond
    ((= opcode 1) +)
    ((= opcode 2) *)
  ))

  (if (= opcode 99)
    (cons false program)
    (cons true (process-opcode call program 1st 2nd 3rd))
  )
)

(define (run-program a b)
  (define program (
    replace-in-list (
      replace-in-list (read) 1 a
    ) 2 b)
  )

  (define (iter output pos)
    (if (equal? (car output) false)
      (cdr output)
      (iter (run-instruction (cdr output) pos) (+ pos 4))
    )
  )

  (car (iter (cons true program) 0))
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
    ((equal? output expected) (cons a b))
    ((< output expected) (run (+ a 1) (+ b 1) expected conv))
    ((> output expected) (run (- a conv) (- b (+ conv 1)) expected (+ conv 1)))
  )
)

(run 0 0 19690720 0)
