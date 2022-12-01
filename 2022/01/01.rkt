#lang racket/base

(require "../../utils.rkt"
         racket/list
         racket/string
         rackunit
         qi)

(define-flow part-1 (~> sep (>< (~> sep +)) collect (sort >) car))
(define-flow part-2 (~> sep (>< (~> sep +)) collect (sort >) (take 3) sep +))

(define-flow string-list->numbers-list (~> (string-split "\n") sep (>< string->number) collect))
(define-flow parser (~> (string-split "\n\n") sep (>< string-list->numbers-list) collect))

(define example (parser "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"))
(define input (parse "input" #:parser parser))

(check-eq? (part-1 example) 24000)
(check-eq? (part-1 input) 70116)

(check-eq? (part-2 example) 45000)
(check-eq? (part-2 input) 206582)
