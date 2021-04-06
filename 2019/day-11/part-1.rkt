#lang racket

(require "../Intcode/runner.rkt")
(require "../Intcode/constants.rkt")
(require "../utils.rkt")

(define directions
  (list (cons 0 -1) ; up
        (cons 1 0) ; right
        (cons 0 1) ; down
        (cons -1 0) ; left
        ))

(define (add-pair a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (get-relative-list-pos lst cur delta)
  (cond
    [(= (+ cur delta) (length lst)) 0]
    [(= (+ cur delta) -1) (- (length lst) 1)]
    [else (+ cur delta)]))

(define (new-coords cur-coords cur-dir update)
  (define delta (if (= update 0) -1 1))

  (define new-dir (modulo (+ cur-dir delta) 4))

  (cons (add-pair cur-coords (list-ref directions new-dir)) new-dir))

(define (panel-color coords coord)
  ; 0 if over black panel, 1 if over white panel
  (if (false? (index-of coords coord)) 0 1))

(define (run-robot runner)
  (define (iter result cur-coords cur-dir white-panels)
    (define input
      ; If current coords are white, give 1, else 0
      (if (set-member? white-panels cur-coords) 1 0))

    (define prog-run (runner input))
    (define out (drop (first prog-run) (- (length (first prog-run)) 2)))
    (define next-coords-dir (new-coords cur-coords cur-dir (second out)))
    (define new-white-panels
      (if (= (first out) 1) (set-add white-panels cur-coords) (set-remove white-panels cur-coords)))

    (cond
      [(finished? runner) result]
      [else (iter (set-add result cur-coords)
                  (car next-coords-dir)
                  (cdr next-coords-dir)
                  new-white-panels)]))

  (iter (set) (cons 0 0) 0 (set)))

(define program
  (flatten (read "Intcode.program"
                 (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(set-count (run-robot (start program)))
