#lang racket

(require "./constants.rkt")
(require "./utils.rkt")

(define (opcode-proc--1 program in out pos rb i j k)
  (list CONTINUE
        (update-program program
                        (translate-write-pair program rb k)
                        (+ (translate-mode-param-pair program rb i)
                           (translate-mode-param-pair program rb j)))
        in
        out
        (+ pos 4)
        rb))

(define (opcode-proc--2 program in out pos rb i j k)
  (list CONTINUE
        (update-program program
                        (translate-write-pair program rb k)
                        (* (translate-mode-param-pair program rb i)
                           (translate-mode-param-pair program rb j)))
        in
        out
        (+ pos 4)
        rb))

(define (opcode-proc--3 program in out pos rb i)
  (if (void? in)
      (list INPUT program (void) out pos rb)
      (list CONTINUE
            (update-program program (translate-write-pair program rb i) in)
            (void)
            out
            (+ pos 2)
            rb)))

(define (opcode-proc--4 program in out pos rb i)
  (define new-output (list (translate-mode-param-pair program rb i)))

  (list CONTINUE program in (append out new-output) (+ pos 2) rb))

(define (opcode-proc--5 program in out pos rb i j)
  (define new-pos
    (if (not (= (translate-mode-param-pair program rb i) 0))
        (translate-mode-param-pair program rb j)
        (+ pos 3)))

  (list CONTINUE program in out new-pos rb))

(define (opcode-proc--6 program in out pos rb i j)
  (define new-pos
    (if (= (translate-mode-param-pair program rb i) 0)
        (translate-mode-param-pair program rb j)
        (+ pos 3)))

  (list CONTINUE program in out new-pos rb))

(define (opcode-proc--7 program in out pos rb i j k)
  (define new-value
    (if (< (translate-mode-param-pair program rb i) (translate-mode-param-pair program rb j)) 1 0))

  (list CONTINUE
        (update-program program (translate-write-pair program rb k) new-value)
        in
        out
        (+ pos 4)
        rb))

(define (opcode-proc--8 program in out pos rb i j k)
  (define new-value
    (if (= (translate-mode-param-pair program rb i) (translate-mode-param-pair program rb j)) 1 0))

  (list CONTINUE
        (update-program program (translate-write-pair program rb k) new-value)
        in
        out
        (+ pos 4)
        rb))

(define (opcode-proc--9 program in out pos rb i)
  (list CONTINUE program in out (+ pos 2) (+ rb (translate-mode-param-pair program rb i))))

(define (opcode-proc--99 program in out pos rb)
  (list HALT program in out pos rb))

(struct opcode (num num-args proc))
(provide (struct-out opcode))

(define opcodes
  (list (opcode 1 3 opcode-proc--1)
        (opcode 2 3 opcode-proc--2)
        (opcode 3 1 opcode-proc--3)
        (opcode 4 1 opcode-proc--4)
        (opcode 5 2 opcode-proc--5)
        (opcode 6 2 opcode-proc--6)
        (opcode 7 3 opcode-proc--7)
        (opcode 8 3 opcode-proc--8)
        (opcode 9 1 opcode-proc--9)
        (opcode 99 0 opcode-proc--99)))

(provide opcodes)
