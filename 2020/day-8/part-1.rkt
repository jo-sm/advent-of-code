#lang racket

(require "../utils.rkt")
(require rackunit)

(struct instruction (operation argument))

(define (list-ref-safe lst i)
  (if (>= i (length lst))
    null
    (list-ref lst i)))

(define (line->instructions line)
  (define-values (operation-name argument) (apply values (string-split line)))

  (instruction operation-name (string->number argument)))

(define (run-instruction instruction visited-positions current-position acc)
  (case (instruction-operation instruction)
    [("nop") (list (cons current-position visited-positions) (+ current-position 1) acc)]
    [("acc") (list (cons current-position visited-positions)
                   (+ current-position 1)
                   (+ acc (instruction-argument instruction)))]
    [("jmp") (list (cons current-position visited-positions)
                   (+ current-position (instruction-argument instruction))
                   acc)]))

(define (run instructions)
  (define (iter visited-positions current-position acc)
    (define current-instruction (list-ref-safe instructions current-position))

    (cond
      [(index-of visited-positions current-position) acc]
      [(null? current-instruction) acc]
      [else (apply iter
                   (run-instruction current-instruction visited-positions current-position acc))]))

  (iter '() 0 0))

(check-eq? (run (read-input-lines #:line-parser line->instructions)) 1810)
