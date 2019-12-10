#lang racket

(require "../utils.rkt")

; Shorthand for coord-angle-distance
(struct cad (coord angle dist))

(define (line-to-xs line)
  (define (iter result i remaining)
    (define update
      (cond
        ((null? remaining) result)
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

(define (z x y) (sqrt (+ (expt x 2) (expt y 2))))

(define (calc-angle x y)
  (define m (- (* (atan y x) (/ 180 pi)) 90))

  (if (negative? m)
    (+ m 360)
    m
  )
)

(define (find-angles coords)
  (define (iter result remaining all)
    (define cur (if (null? remaining) null (car remaining)))

    (if (null? remaining)
      result
      (iter
        (cons
          (cons
            cur
            (map
              (lambda
                (i)
                (if (equal? i cur)
                  (cad i 0 0)
                  (let ([x (- (car cur) (car i))] [y (- (cdr cur) (cdr i))])
                    (cad i (calc-angle x y) (z x y))
                  )
                )
              )
              all
            )
          )
          result
        )
        (cdr remaining)
        all
      )
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

(define (run cads)
  (define (iter i j remaining)
    ; This would break in the general case (e.g. if we wanted to iterate through every item)
    ; but since we specifically want 200, and our input here is longer than 200, it's okay.
    (define next-j (if (= (+ j 1) (length remaining)) 0 (+ j 1)))
    (define cur (car (list-ref remaining j)))

    (cond
      ((= i 200) (first cur))
      (else (iter (+ i 1) next-j (list-set remaining j cur))))
    )
  )

  (iter 0 0 cads)
)

(define raw (read "input" line-to-xs))
(define all (find-angles (generate-coords raw)))
(define most-visible (car
  (foldr
    (lambda (i memo) (if (> (set-count (cdr i)) (set-count (cdr memo))) i memo))
    (cons (void) (set))
    (map
      (lambda (i) (cons (car i) (list->set (map cad-angle (cdr i)))))
      (find-angles (generate-coords raw))
    )
  )
))
(define cads (cdr (findf (lambda (i) (equal? (car i) most-visible)) all)))
(define grouped-by-angle (sort (group-by cad-angle cads) (lambda (i j) (< (cad-angle (first i)) (cad-angle (first j))))))
(define grouped-by-angle-distance (map (lambda (i) (sort i (lambda (j k) (<= (cad-dist j) (cad-dist k))))) by-angle))
(define final (run grouped-by-angle-distance))

(+ (* 100 (car (cad-coord final))) (cdr (cad-coord final)))
