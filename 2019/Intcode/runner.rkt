#lang racket

(require racket/generator)
(require "./constants.rkt")
(require "./opcodes.rkt")

; TODO: define "get-interpreted-arg" with program as input that works like
; "translate-mode-param-pair"
(struct arg-with-mode (argument mode))

(define (number->digits i)
  (map char->number (string->list (number->string i)))
)

(define (char->number c)
  (- (char->integer c) (char->integer #\0))
)

(define (get-opcode program pos)
  (define stringified-opcode (number->string (list-ref program pos)))
  (define num (string->number (substring stringified-opcode (max 0 (- (string-length stringified-opcode) 2)))))

  (first (filter (lambda (i) (= num (opcode-num i))) opcodes))
)

(define (get-modes program pos)
  (define digits (number->digits (list-ref program pos)))

  (if (> (length digits) 2)
    (take digits (- (length digits) 2))
    null
  )
)

(define (translate-arg-pos-to-args program pos args-pos)
  (define parameters (get-modes program pos))

  (define (iter result remaining-params args-pos)
    (cond
      ((null? args-pos) (reverse result))
      ((null? remaining-params) (iter (cons (cons 0 (car args-pos)) result) null (cdr args-pos)))
      ((= (car remaining-params) 0) (iter (cons (cons 0 (car args-pos)) result) (cdr remaining-params) (cdr args-pos)))
      ((= (car remaining-params) 1) (iter (cons (cons 1 (car args-pos)) result) (cdr remaining-params) (cdr args-pos)))
      ((= (car remaining-params) 2) (iter (cons (cons 2 (car args-pos)) result) (cdr remaining-params) (cdr args-pos)))
    )
  )

  (iter null (reverse parameters) args-pos)
)

(define (run-instruction program input output pos rb)
  (define opcode (get-opcode program pos))
  (define args-pos (take (drop program (+ pos 1)) (opcode-num-args opcode)))
  (define args (translate-arg-pos-to-args program pos args-pos))

  (apply (opcode-proc opcode) program input output pos rb args)
)

(define (start program)
  (define r (generator ()
    ; initial conditions
    (let loop ([result (list CONTINUE program (void) null 0 0)])
      (cond
        ((equal? (first result) HALT) (list (fourth result) (second result)))
        ((equal? (first result) CONTINUE) (loop (apply run-instruction (cdr result))))
        (else
          (let ([input (yield (list (fourth result)))])
            (loop (run-instruction (second result) input (fourth result) (fifth result) (sixth result)))
          )
        )
      )
    )
  ))

  (r)

  r
)
(provide start)

(define (finished? intcode-program)
  (equal? (generator-state intcode-program) 'done)
)
(provide finished?)
