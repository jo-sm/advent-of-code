#lang racket
; Valid opcodes: 1, 2, 99

; Opcode 1: Addition.
; Given A, B, C, D, where A is 1, B, C, and D are positions of the given Intcode program.
; Find the values at positions B and C, add them, and put the output into position D (overwriting
; the existing value in position D).
;
; Opcode 2: Multiplication.
; Given A, B, C, D, where A is 2, B, C, and D are positions of the given Intcode program.
; Find the values at positions B and C, multiply them, and put the output into position D.
;
; Opcode 99: Stop.

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

(define (run-program)
  (define program (
    replace-in-list (
      replace-in-list (read) 1 12
    ) 2 2)
  )

  (define (iter output pos)
    (if (equal? (car output) false)
      (cdr output)
      (iter (run-instruction (cdr output) pos) (+ pos 4))
    )
  )

  (iter (cons true program) 0)
)

(run-program)
