#lang racket

(require "../utils.rkt")
(require "../Intcode/runner.rkt")

(define program (flatten (read "Intcode.program" (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(define (generate-signals)
  (define (determine-signal-for-combo prev-output combo)
    (if (null? combo)
      prev-output
      (let ([run-one (run-intcode-program program #:input (car combo))])
        (determine-signal-for-combo (first (fourth (run-intcode-program (second run-one) #:input prev-output #:output (fourth run-one) #:pos (fifth run-one)))) (cdr combo))
      )
    )
  )

  (define (iter result combos)
    (if (null? combos)
      result
      (iter (cons (determine-signal-for-combo 0 (car combos)) result) (cdr combos))
    )
  )

  (iter null (permutations (list 0 1 2 3 4)))
)

(foldr (lambda (i memo) (if (> i memo) i memo)) 0 (generate-signals))
