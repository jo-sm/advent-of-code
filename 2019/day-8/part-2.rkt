#lang racket
(require "../utils.rkt")

(define HEIGHT 6)
(define WIDTH 25)

(define data (first (read "input")))

(define (data->layers data)
  (define (iter result rest)
    (if (equal? rest "")
      (reverse result)
      (iter (cons (substring rest 0 (* HEIGHT WIDTH)) result) (substring rest (* HEIGHT WIDTH)))))

  (iter null data))

(define (layers->pixels layers)
  (define (iter result i)
    (if (= (string-length (first layers)) i)
      (reverse result)
      (iter (cons (map (lambda (layer) (substring layer i (+ i 1))) layers) result) (+ i 1))))

  (iter null 0))

(define (pixel->color pixel)
  (if (equal? pixel "0")
    (get-color "black")
    (get-color "white")))

(define (get-first-opaque-pixel possible)
  (findf (lambda (i) (not (equal? i "2")))
    possible))

(define layers (data->layers data))
(define pixels (layers->pixels layers))
(define lines (split-by (map pixel->color (map get-first-opaque-pixel pixels)) WIDTH))

(create-jpg "output.jpg" lines #:scaling-factor 10)
