#lang racket

(require "../utils.rkt")

; Helpers for coords/vectors
(define (cur-coord coords) (car coords))
(define (cur-vec vectors) (car vectors))
(define (cur-vec-direction vectors) (car (car vectors)))
(define (cur-vec-length vectors) (cdr (car vectors)))
(define (subtract-one-from-vec v) (cons (car v) (- (cdr v) 1)))

(define (line-to-vectors line)
  (map
    (lambda (i) (cons (substring i 0 1) (string->number (substring i 1))))
    (string-split line ",")
  )
)

(define (calc-manhattan-dist coord)
  ; Since we are always measuring the distance from 0,0 we can simply add the absolute values together
  ; The general case: |p1-q1|+|p2-q2|
  (+ (abs (car coord)) (abs (cdr coord)))
)

(define (get-lowest-value lst)
  (foldr (lambda (i memo) (if (< i memo) i memo)) (car lst) lst)
)

(define (calc-next-coord vector cur-coord)
  (define direction (car vector))
  (define i (car cur-coord))
  (define j (cdr cur-coord))

  (cond
    ((equal? direction "R") (cons i (+ j 1)))
    ((equal? direction "L") (cons i (- j 1)))
    ((equal? direction "D") (cons (- i 1) j))
    ((equal? direction "U") (cons (+ i 1) j))
  )
)

(define (calc-coords-from-vecs vecs)
  (define (iter coords remaining-vecs)
    (cond
      ((null? remaining-vecs) coords)

      ; If the remaining length of the current vector is 0, continue with the next vector
      ((= (cdr (car remaining-vecs)) 0) (iter coords (cdr remaining-vecs)))

      ; Iterate by prepending the next coordinate to the coords list, and subtracing one from current vector
      (else
        (iter
          (cons (calc-next-coord (cur-vec remaining-vecs) (cur-coord coords)) coords)
          (cons (subtract-one-from-vec (cur-vec remaining-vecs)) (cdr remaining-vecs))
        )
      )
    )
  )

  ; Initial coordinates
  (iter (list (cons 0 0)) vecs)
)

(define (find-intersections p q)
  (set->list (set-remove (set-intersect (list->set p) (list->set q)) (cons 0 0)))
)

(define (calc-length-from-start-of-path coord coords)
  (- (- (length coords) (index-of coords coord)) 1)
)

(define paths-vectors
  (let ([lines (read "input" line-to-vectors)])
    (cons (car lines) (car (cdr lines)))
  )
)

(define paths-coords (cons
  (calc-coords-from-vecs (car paths-vectors))
  (calc-coords-from-vecs (cdr paths-vectors))
))

(define intersecting-path-coords (find-intersections (car paths-coords) (cdr paths-coords)))

(define distances-from-start
  (map
    (lambda (coord)
      (cons
        (calc-length-from-start-of-path coord (car paths-coords))
        (calc-length-from-start-of-path coord (cdr paths-coords))
      )
    )
    intersecting-path-coords
  )
)

(get-lowest-value (map add-pair distances-from-start))
