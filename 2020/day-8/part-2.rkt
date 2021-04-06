#lang racket

(require "../utils.rkt")
(require srfi/26)
(require rackunit)

(struct instruction (operation argument))

(define (list-ref-safe lst i)
  (if (>= i (length lst)) null (list-ref lst i)))

(define (line->instructions line)
  (define-values (operation-name argument) (apply values (string-split line)))

  (instruction operation-name (string->number argument)))

(define (run-instruction instruction instruction-set visited-positions current-position acc)
  (case (instruction-operation instruction)
    [("nop")
     (list instruction-set (cons current-position visited-positions) (+ current-position 1) acc)]
    [("acc") (list instruction-set
                   (cons current-position visited-positions)
                   (+ current-position 1)
                   (+ acc (instruction-argument instruction)))]
    [("jmp") (list instruction-set
                   (cons current-position visited-positions)
                   (+ current-position (instruction-argument instruction))
                   acc)]))

(define (run-instruction-set instruction-set visited-positions current-position acc)
  (define current-instruction (list-ref-safe instruction-set current-position))

  (cond
    [(index-of visited-positions current-position) null]
    [(null? current-instruction) acc]
    [else
     (apply
      run-instruction-set
      (run-instruction current-instruction instruction-set visited-positions current-position acc))]))

(define (swap-jmp-nop instructions pos)
  (define specified-instruction (list-ref instructions pos))

  (if (equal? (instruction-operation specified-instruction) "jmp")
      (list-set instructions pos (instruction "nop" (instruction-argument specified-instruction)))
      (list-set instructions pos (instruction "jmp" (instruction-argument specified-instruction)))))

(define (run instructions)
  (define (iter jmp-nop-instruction-positions)
    (define instruction-set (swap-jmp-nop instructions (car jmp-nop-instruction-positions)))
    (define program-run (run-instruction-set instruction-set '() 0 0))

    (if (null? program-run) (iter (cdr jmp-nop-instruction-positions)) program-run))

  (define jmp-nop-instruction-positions
    (indexes-where instructions
                   (lambda (inst)
                     (or (equal? (instruction-operation inst) "nop")
                         (equal? (instruction-operation inst) "jmp")))))

  (iter jmp-nop-instruction-positions))

(check-eq? (run (read-input-lines #:line-parser line->instructions)) 969)
