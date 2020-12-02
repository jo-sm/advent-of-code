#lang racket

(require "../utils.rkt")

(define (list-head lst)
  (reverse (rest (reverse lst)))
)

(define (find-2020 lst)
  (define (iter remaining)
    (cond
      ((< (+ (car remaining) (last remaining)) 2020) (iter (cdr remaining)))
      ((> (+ (car remaining) (last remaining)) 2020) (iter (list-head remaining)))
      (else (list (car remaining) (last remaining)))
    )
  )

  (iter lst)
)

(define (main numbers)
  (let* (
    [sorted (reverse (sort numbers <))]
    [smallest (last sorted)]
    [filtered (filter (lambda (x) (< (+ smallest x) 2020)) sorted)]
  )
    (apply * (find-2020 (reverse filtered)))
  )
)

(main (read "input" string->number))
