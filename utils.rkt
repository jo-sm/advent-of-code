#lang racket/base

(require racket/contract
         racket/function
         racket/port
         racket/string
         racket/list
         racket/bool
         srfi/26)

(provide (all-defined-out))

(define/contract (parse filename [as-lines? #f] #:parser [parser identity])
  (->* (string?) (boolean? #:parser (-> any/c any/c)) any/c)
  (define raw (port->string (open-input-file filename #:mode 'text)))
  (if as-lines?
    (map parser (string-split raw "\n"))
    (parser raw)))

(define/contract (transpose matrix)
  (-> (listof (listof any/c)) (listof (listof any/c)))
  (for/list ([i (range (length matrix))])
    (map (λ (row)
           (list-ref row i))
         matrix)))

(define/contract (mapmap pred lists)
  (-> (-> any/c any/c) (listof (listof any/c)) (listof (listof any/c)))
  (map (λ (lst)
         (map pred lst))
       lists))

(define/contract (chunk lst n)
  (-> (listof any/c) exact-integer? (listof (listof any/c)))
  (let loop ([remaining lst] [result '()])
    (cond
      [(null? remaining) (reverse result)]
      [(> n (length remaining)) (loop '() (cons remaining result))]
      [else (loop (drop remaining n) (cons (take remaining n) result))])))

(define/contract (true? v)
  (-> any/c boolean?)
  (not (false? v)))

(define/contract (char->number char)
  (-> char? number?)
  (- (char->integer char) 48))

(define/contract (matrix-ref matrix x y)
  (-> (listof (listof number?)) number? number? any/c)
  (list-ref (list-ref matrix y) x))
