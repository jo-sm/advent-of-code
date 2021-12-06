#lang racket/base

(require "../utils.rkt"
         threading
         racket/string
         racket/list
         rackunit)

#|
Yet another tricky day. The concept is relatively straightforward - memoize the functions - but I ended up only memoizing `simulate`
and not `nested-length` for a while, which worked but was unnecessarily slow (calling `(simulate 6 256)` took ~300ms but wrapping it in
`nested-length` caused the time to explode to ~60s.

I don't know if it would be possible to avoid the use of `nested-length` and keep a count internal to `simulate` due to the way memoization
works, but it's worth exploring.
|#

(define (memoize proc)
  (define cache (make-hash))

  (define (call . args)
    (if (hash-has-key? cache args)
      (hash-ref cache args)
      (let ([result (apply proc args)])
        (hash-set! cache args result)
        result)))
  call)

(define nested-length
  (memoize (λ (lst n)
             (cond
               [(empty? lst) n]
               [(list? (car lst)) (nested-length (cdr lst) (+ (nested-length (car lst) 0) n))]
               [else (nested-length (cdr lst) (add1 n))]))))

(define simulate
  (memoize (λ (curr n)
             (cond
               [(= n 0) curr]
               [(= curr 0) (list (simulate 6 (sub1 n)) (simulate 8 (sub1 n)))]
               [else (simulate (sub1 curr) (sub1 n))]))))

(define (part-1 input)
  (~>> input
       (map (λ (n)
              (simulate n 80)))
       (nested-length _ 0)))

(define (part-2 input)
  (~>> input
       (map (λ (n)
              (simulate n 256)))
       (nested-length _ 0)))

(define (parser raw)
  (~> raw
      string-trim
      (string-split ",")
      (map string->number _)))

(define example (parser "3,4,3,1,2"))
(define input (parse "06.rktd" #:parser parser))

(check-eq? (part-1 example) 5934)
(check-eq? (part-1 input) 373378)

(check-eq? (part-2 example) 26984457539)
(check-eq? (part-2 input) 1682576647495)
