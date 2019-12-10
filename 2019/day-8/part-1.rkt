#lang racket

(define HEIGHT 6)
(define WIDTH 25)

(require "../utils.rkt")

(define data (first (read "input")))

(define (create-image-layers-from-data data)
  (define (iter result rest)
    (if (equal? rest "")
      result
      (iter (cons (substring rest 0 (* HEIGHT WIDTH)) result) (substring rest (* HEIGHT WIDTH)))
    )
  )

  (iter null data)
)

(define (count-num-digits piece num)
  (define str-num (number->string num))

  (define (iter result rest)
    (cond
      ((equal? rest "") result)
      ((equal? (substring rest 0 1) str-num) (iter (+ result 1) (substring rest 1)))
      (else (iter result (substring rest 1)))
    )
  )

  (iter 0 piece)
)

(define each-num-zeros (map (lambda (i) (cons (count-num-digits i 0) i)) (create-image-layers-from-data data)))
(define layer-with-most (cdr (foldr (lambda (i memo) (if (<= (car i) (car memo)) i memo)) (cons +inf.0 "") each-num-zeros)))

(* (count-num-digits layer-with-most 1) (count-num-digits layer-with-most 2))
