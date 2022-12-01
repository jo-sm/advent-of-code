#lang racket/base

(require "../utils.rkt"
         threading
         racket/match
         racket/function
         rackunit)

(define (middle lst)
  (list-ref lst (floor (/ (length lst) 2))))

(define (hash-key-by-value hash value)
  (~>> hash
       hash->list
       (findf (λ (pair)
                (eq? (cdr pair) value))
         )
       car))

(define all-chars (make-hash (list (cons #\) #\() (cons #\] #\[) (cons #\} #\{) (cons #\> #\<))))
(define opening-chars (hash-values all-chars))
(define closing-chars (hash-keys all-chars))

(define (first-illegal-char chars)
  (let loop ([stack '()] [rem chars])
    (cond
      [(null? rem) stack]
      [(null? stack) (loop (cons (car rem) stack) (cdr rem))]
      [(member (car rem) opening-chars) (loop (cons (car rem) stack) (cdr rem))]
      [(and (member (car rem) closing-chars)
            (char=? (car stack) (hash-ref all-chars (car rem))))
       (loop (cdr stack) (cdr rem))]
      [(member (car rem) closing-chars) (car rem)]
      [else (loop stack (cdr rem))])))

(define (illegal-char-score char)
  (match char
    [#\) 3]
    [#\] 57]
    [#\} 1197]
    [#\> 25137]))

; TODO why can't I use match with a variable definition, only function def?
(define (incomplete-char-score char)
  (match char
    [#\) 1]
    [#\] 2]
    [#\} 3]
    [#\> 4]))

(define (incomplete-chunk-score chars)
  (for/fold ([score 0])
    ([char (in-list chars)])

    (+ (incomplete-char-score char) (* 5 score))))

(define (part-1 input)
  (~>> input
       (map first-illegal-char)
       (filter char?)
       (map illegal-char-score)
       (apply +)))

(define (part-2 input)
  (~>> input
       (map first-illegal-char)
       (filter list?)
       (mapmap (curry hash-key-by-value all-chars))
       (map incomplete-chunk-score)
       (sort _ <)
       middle))

(define example
  (map string->list
       (list "[({(<(())[]>[[{[]{<()<>>"
             "[(()[<>])]({[<{<<[]>>("
             "{([(<{}[<>[]}>{[]{[(<()>"
             "(((({<>}<{<{<>}{[]{[]{}"
             "[[<[([]))<([[{}[[()]]]"
             "[{[{({}]{}}([{[{{{}}([]"
             "{<[[]]>}<{[{[{[]{()[[[]"
             "[<(<(<(<{}))><([]([]()"
             "<{([([[(<>()){}]>(<<{{"
             "<{([{{}}[<[[[<>{}]]]>[]]")))
(define input (parse "10.rktd" #t #:parser string->list))

(check-eq? (part-1 example) 26397)
(check-eq? (part-1 input) 392367)

(check-eq? (part-2 example) 288957)
(check-eq? (part-2 input) 2192104158)
