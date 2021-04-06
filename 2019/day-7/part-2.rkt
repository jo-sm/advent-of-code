#lang racket

(require "../utils.rkt")
(require "../Intcode/constants.rkt")
(require "../Intcode/runner.rkt")

(define program
  (flatten (read "Intcode.program"
                 (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(define (first-run i ps combo-digit prev-output)
  (define p (list-ref ps i))

  ; Run the first time with the phase param
  (p combo-digit)

  ; run again
  (first (p prev-output)))

(define (normal-run i ps prev-output)
  (define p (list-ref ps i))

  (first (p prev-output)))

(define (generate-signals)
  (define (determine-signal-for-combo i ps combo prev-output)
    (define next-prev-output
      (if (null? combo) (normal-run i ps prev-output) (first-run i ps (car combo) prev-output)))

    (define next-combo (if (null? combo) null (cdr combo)))
    (define next-i (modulo (+ i 1) 5))

    (cond
      [(finished? (list-ref ps 4)) (last next-prev-output)]
      [else (determine-signal-for-combo next-i ps next-combo (last next-prev-output))]))

  (define (generate-initial-programs)
    (list (start program) (start program) (start program) (start program) (start program)))

  (define (iter result combos)
    (if (null? combos)
        result
        (iter (cons (determine-signal-for-combo 0 (generate-initial-programs) (car combos) 0) result)
              (cdr combos))))

  (iter null (permutations (list 5 6 7 8 9))))

(foldr (lambda (i memo) (if (> i memo) i memo)) 0 (generate-signals))
