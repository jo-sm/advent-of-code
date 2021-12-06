#lang racket/base

#|
  Huge learning for me in this one, and the first day where there was a real chalenge that I
  couldn't solve by somehow being creative or sussing the problem out. Part one was pretty
  straightforward, and, as usual, the most challenging part was figuring out the best way
  to lay out the program.

  This one, however, was really tricky, because it became immediately apparent that letting
  the program run wouldn't work and I didn't know where to go because I had a working solution
  for the example case. My first approach was to see if there was some mathematical way to do it,
  using some kind of combinatorics math, but my search was fruitless and my on-the-paper attempt
  also proved fruitless. Eventually I looked up in the company Slack channel about this day and
  saw people had used memoization, and after some additional research found a basic memoization
  procedure, which when used, gave me the correct solution.
|#

(require rackunit)
(require racket/list)
(require racket/function)
(require srfi/26)
(require "../utils.rkt")

(define (list-ref-safe lst i)
  (if (>= i (length lst))
    null
    (list-ref lst i)))

(define (cdr-until-false lst proc)
  (cond
    [(null? lst) null]
    [(proc lst) (cons lst (cdr-until-false (cdr lst) proc))]
    [else null]))

(define (memoize fn)
  (let ([cache (make-hash)]) (lambda arg (hash-ref! cache arg (thunk (apply fn arg))))))

(define (count-possible-adapter-configurations raw-joltages)
  (define adapter-joltages (sort raw-joltages <))
  (define all-joltages (append adapter-joltages (list (+ (last adapter-joltages) 3))))

  (define iter
    (memoize (lambda (current remaining-joltages)
               (define actual-next
                 (cdr-until-false remaining-joltages (lambda (i) (<= (- (car i) current) 3))))

               (cond
                 [(null? remaining-joltages) 1]
                 [(= (length actual-next) 0) 0]
                 [else (apply + (map (lambda (i) (iter (car i) (cdr i))) actual-next))]))))

  (iter 0 all-joltages))

(define (count-stuff remaining [i 0])
  (cond
    [(= (length remaining) 1) (+ i 1)]
    [else (count-stuff (cdr remaining)
                       (+ i -1 (count (lambda (i) (<= (- i (car remaining)) 3)) (cdr remaining))))]))

(check-eq?
 (count-possible-adapter-configurations (read-input-lines "example" #:line-parser string->number))
 19208)

(count-possible-adapter-configurations (read-input-lines #:line-parser string->number))
