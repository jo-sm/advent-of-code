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

Update: there is a closed form of the function, which is:

Given f(m, n):
1 if m <= n
2 if m < 9
f(n-9) + f(n-7) otherwise

I saw something about it while looking at other solutions on Reddit, but the post didn't go into much detail and I can't find it now to see if
it's been updated. It didn't clearly state the closed form, but said that f(8) = f(0) + f(2) which made me think a bit more about the general
case, coming up with the above. I plan to look into how I could have come up with this by analysis.
|#

; TODO: turn into syntax, like `define/memo` (I know there is the `memoize` package but I want to do it myself :-))
(define (memoize proc)
  (define cache (make-hash))

  (define (call . args)
    (if (hash-has-key? cache args)
      (hash-ref cache args)
      (let ([result (apply proc args)])
        (hash-set! cache args result)
        result)))
  call)

(define simulate
  (memoize (λ (m n)
             (cond
               [(<= m n) 1]
               [(< m 9) 2]
               [else (+ (simulate (- m 9) n) (simulate (- m 7) n))]))))

(define (part-1 input)
  (~>> input
       (map (λ (n)
              (simulate 80 n)))
       (apply +)))

(define (part-2 input)
  (~>> input
       (map (λ (n)
              (simulate 256 n)))
       (apply +)))

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
