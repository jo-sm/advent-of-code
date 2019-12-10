#lang racket

(require "../utils.rkt")

(define (generate-path end start lst)
  (define (iter result)
    (if (equal? (first result) end)
      result
      (iter (cons (findf (lambda (i) (equal? (first (first result)) (second i))) lst) result))
    )
  )

  (iter (list start))
)

(define (nearest-neighbor a b end lst)
  (define path-a (generate-path end a orbits))
  (define path-b (generate-path end b orbits))

  (define (iter a b)
    (define cur-a (first a))
    (define cur-b (first b))
    (define next-a (second a))
    (define next-b (second b))

    (if (and (equal? cur-a cur-b) (equal? next-a next-b))
      (iter (cdr a) (cdr b))
      cur-a
    )
  )

  (iter path-a path-b)
)

(define orbits (read "input" (lambda (line) (string-split line ")"))))
(define COM (findf (lambda (i) (equal? (first i) "COM")) orbits))
(define SAN (findf (lambda (i) (equal? (second i) "SAN")) orbits))
(define YOU (findf (lambda (i) (equal? (second i) "YOU")) orbits))
(define closest-same (nearest-neighbor SAN YOU COM orbits))

(define path-closest-you (generate-path closest-same YOU orbits))
(define path-closest-santa (generate-path closest-same SAN orbits))

#|
  (("A" "B") ("B" "C") ("C" "D") ("D" "YOU"))
  (("A" "B") ("B" "T") ("T" "V") ("V" "Z") ("Z" "SAN"))

  This would be the path generated from YOU and SAN to the nearest neighbor,
  in this case ("A" "B"). To get the number of orbital transfers between the
  closest object YOU and SAN are orbiting, we can remove YOU and SAM, and
  because ("B" "C") and ("B" "T") can jump between each other, we can ignore
  the first item of each path as well.
|#
(- (length (append path-closest-you path-closest-santa)) 4)

