#lang racket

(require "../utils.rkt")
(require "../Intcode/constants.rkt")
(require "../Intcode/runner.rkt")

(define program (flatten (read "Intcode.program" (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(define (first-run i ps combo-digit)
  (define last-i (if (= i 0) 4 (- i 1)))
  (define p (list-ref ps i))
  (define last-p (list-ref ps last-i))

  ; Run the first time with the phase param
  (define 1st (run-intcode-program (second p) #:input combo-digit #:output (fourth p) #:pos (fifth p)))

  ; Run again
  (run-intcode-program (second 1st) #:input (last (fourth last-p)) #:output (fourth 1st) #:pos (fifth 1st))
)

(define (normal-run i ps)
  (define last-i (if (= i 0) 4 (- i 1)))
  (define p (list-ref ps i))
  (define last-p (list-ref ps last-i))

  (run-intcode-program (second p) #:input (last (fourth last-p)) #:output (fourth p) #:pos (fifth p))
)

(define (generate-signals)
  (define (determine-signal-for-combo i ps combo)
    (define n (if (null? combo)
      (normal-run i ps)
      (first-run i ps (car combo))
    ))

    (define next-combo (if (null? combo) null (cdr combo)))
    (define next-i (if (= i 4) 0 (+ i 1)))

    (cond
      ((and (= i 4) (equal? (first n) HALT)) (last (fourth n)))
      (else (determine-signal-for-combo next-i (list-set ps i n) next-combo))
    )
  )

  (define (create-initial-programs combo)
    (list
      (list CONTINUE program (void) null 0)
      (list CONTINUE program (void) null 0)
      (list CONTINUE program (void) null 0)
      (list CONTINUE program (void) null 0)
      (list CONTINUE program (void) (list 0) 0)
    )
  )

  (define (iter result combos)
    (if (null? combos)
      result
      (iter (cons (determine-signal-for-combo 0 (create-initial-programs (car combos)) (car combos)) result) (cdr combos))
    )
  )

  (iter null (permutations (list 5 6 7 8 9)))
)

(foldr (lambda (i memo) (if (> i memo) i memo)) 0 (generate-signals))
