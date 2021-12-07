#lang racket/base

(require "../utils.rkt"
         threading
         racket/string
         racket/list
         rackunit)

#|
Very simple day today (compared to the last few). Not much to be said, except that using `in-range` is significantly faster than `range`
with large numbers. That is, the following:

```
; cpu time: 10500 real time: 11167 gc time: 157
(apply + (range 1 (+ n 1)))
```

is significantly slower than this:

```
; cpu time: 1592 real time: 1740 gc time: 10
(for/fold ([result 0])
    ([i (in-range 1 (+ n 1))])
    (+ result i))
```

But, in this case, it's a lot better to just use `n * n + 1 / 2`. It's much faster than either.

Another thing that could likely improve things is to avoid using 3 maps. It's not like Clojure where they don't make an intermediate
list, so it's probably a bit slower due to that.
|#

(define (triangular n)
  (/ (* n (+ n 1)) 2))

(define (calculate-simple-move-costs nums)
  (define floor (apply min nums))
  (define ceil (apply max nums))

  (for/list ([i (in-range floor ceil)])
    (~>> nums
         (map (λ (n)
                (- n i)))
         (map abs)
         (apply +))))

(define (calculate-complex-move-costs nums)
  (define floor (apply min nums))
  (define ceil (apply max nums))

  (for/list ([i (in-range floor ceil)])
    (~>> nums
         (map (λ (n)
                (- n i)))
         (map abs)
         (map triangular)
         (apply +))))

(define (part-1 input)
  (apply min (calculate-simple-move-costs input)))

(define (part-2 input)
  (apply min (calculate-complex-move-costs input)))

(define (parser raw)
  (~> raw
      string-trim
      (string-split ",")
      (map string->number _)))
(define example (parser "16,1,2,0,4,2,7,1,2,14"))
(define input (parse "07.rktd" #:parser parser))

(check-eq? (part-1 example) 37)
(check-eq? (part-1 input) 326132)

(check-eq? (part-2 example) 168)
(check-eq? (part-2 input) 88612508)
