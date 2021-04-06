#lang racket

(require "../utils.rkt")

(define (mass-fuel-req mass)
  (- (floor (/ mass 3)) 2))

(apply + (map mass-fuel-req (read "fuel-input.txt" string->number)))
