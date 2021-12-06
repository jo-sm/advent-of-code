#lang racket/base

(require "./utils.rkt"
         threading
         racket/string
         racket/list
         rackunit
         srfi/26)

(define (dimensions->sides dimensions)
  (~>> (string-split dimensions "x") (map string->number)))

(define (dimensions->sqft dimensions)
  (define surface-areas (~>> dimensions dimensions->sides (combinations _ 2) (map (cut apply * <>))))
  (define smallest (car (sort surface-areas <)))

  (~> surface-areas (map (cut * 2 <>) _) (apply + _) (+ smallest)))

(define (dimensions->ribbon-length dimensions)
  (define sides (~> (dimensions->sides dimensions) (sort <)))

  (+ (* 2 (car sides)) (* 2 (cadr sides)) (apply * sides)))

(define (part-1 input)
  (~>> input (map dimensions->sqft) (apply +)))

(define (part-2 input)
  (~>> input (map dimensions->ribbon-length) (apply +)))

(define example '("2x3x4" "1x1x10"))
(define input (read-input-lines "02.rktd"))

(check-eq? (part-1 example) (+ 58 43))
(check-eq? (part-1 input) 1588178)

(check-eq? (part-2 example) (+ 34 14))
(check-eq? (part-2 input) 3783758)
