#lang racket

(require racket/draw)
(require racket/gui/base)

(define (read [filename "input"] [mapf identity])
  (define lines (port->lines (open-input-file filename #:mode 'text)))

  (map mapf lines)
)
(provide read)

(define (add-pair pair) (+ (car pair) (cdr pair)))
(provide add-pair)

(define (create-jpg filename lines #:scaling-factor [scaling-factor 2.0])
  (define height (length lines))
  (define width (length (first lines)))
  (define target (make-object bitmap% width height false false scaling-factor))
  (define dc (new bitmap-dc% [bitmap target]))

  (define x 0)
  (define y 0)

  (for-each (lambda (line) (begin
    (for-each (lambda (pixel) (begin
      (send dc set-pixel x y pixel)

      (set! x (+ x 1))
    )) line)

    (set! x 0)
    (set! y (+ y 1))
  )) lines)

  (send target save-file filename 'jpeg #:unscaled? true)
)
(provide create-jpg)

(define (get-color name)
  (send the-color-database find-color name)
)
(provide get-color)

(define (split-by lst i)
  (define (iter result rest)
    (if (null? rest)
      (reverse result)
      (iter (cons (take rest i) result) (drop rest i))
    )
  )

  (iter null lst)
)
(provide split-by)
