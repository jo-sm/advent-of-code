#lang racket

(require "../utils.rkt")

(define (line-to-xs line)
  (define (iter result i remaining)
    (define update
      (cond
        ((null? remaining) result)

        ; Subtract 1 here since string-split pre- and appends "" to the list
        ((equal? "#" (car remaining)) (cons (- i 1) result))
        (else result)
      )
    )

    (if (null? remaining)
      result
      (iter update (+ i 1) (cdr remaining))
    )
  )

  (iter null 0 (string-split line ""))
)

(define (find-angles coords)
  (define (iter result remaining all)
    (define cur (if (null? remaining) null (car remaining)))

    (if (null? remaining)
      result
      (iter (cons (cons cur (list->set (map (lambda (i) (if (equal? i cur) 0 (atan (- (car cur) (car i)) (- (cdr cur) (cdr i))))) all))) result) (cdr remaining) all)
    )
  )

  (iter null coords coords)
)

(define (generate-coords raw)
  (define (iter result i remaining)
    (if (null? remaining)
      result
      (iter (append (map (lambda (j) (cons j i)) (car remaining)) result) (+ i 1) (cdr remaining))
    )
  )

  (iter null 0 raw)
)

(define raw (read "input" line-to-xs))

(foldr (lambda (i memo) (if (> i memo) i memo)) 0 (map (lambda (i) (set-count (cdr i))) (find-angles (generate-coords raw))))
