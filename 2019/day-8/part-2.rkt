#lang racket
(require "../utils.rkt")
(require racket/draw)
(require racket/gui/base)

(define HEIGHT 6)
(define WIDTH 25)

(define data (first (read "input")))

(define (data->layers data)
  (define (iter result rest)
    (if (equal? rest "")
      result
      (iter (cons (substring rest 0 (* HEIGHT WIDTH)) result) (substring rest (* HEIGHT WIDTH)))
    )
  )

  (iter null data)
)

(define (get-pixel layers i)
  (first (filter (lambda (i) (not (equal? i "2"))) (reverse (foldr (lambda (j memo) (cons (substring j i (+ i 1)) memo)) null layers))))
)

(define layers (data->layers data))
(define WHITE (send the-color-database find-color "White"))
(define BLACK (send the-color-database find-color "Black"))

(define (pixel-to-color pixel)
  (cond
    ((equal? pixel "0") BLACK)
    ((equal? pixel "1") WHITE)
  )
)

(define (image pixels)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (cons (take rest WIDTH) result) (drop rest WIDTH))
    )
  )

  (reverse (iter null pixels))
)

(define (create-image dc data)
  (define (iter x y)
    (cond
      ((and (= (+ x 1) WIDTH) (= (+ y 1) HEIGHT)) (void))
      ((= x 24) (begin (send dc set-pixel x y (pixel-to-color (list-ref (list-ref data y) x))) (iter 0 (+ y 1))))
      (else (begin (send dc set-pixel x y (pixel-to-color (list-ref (list-ref data y) x))) (iter (+ x 1) y)))
    )
  )

  (iter 0 0)
)

(define target (make-object bitmap% WIDTH HEIGHT false false 10.0))
(define dc (new bitmap-dc% [bitmap target]))

(create-image dc (image (map (lambda (i) (get-pixel layers i)) (range 0 (* HEIGHT WIDTH)))))

(send target save-file "output.jpg" 'jpeg #:unscaled? true)
