#lang racket

(define (get-from-program program pos)
  (if (>= pos (length program))
    0
    (list-ref program pos)))

(define (translate-mode-param-pair program rb param-mode-pair)
  (define mode (car param-mode-pair))
  (define param (cdr param-mode-pair))

  (cond
    [(= mode 0) (get-from-program program param)]
    [(= mode 1) param]
    [(= mode 2) (get-from-program program (+ param rb))]))

(provide translate-mode-param-pair)

(define (translate-write-pair program rb param-mode-pair)
  (define mode (car param-mode-pair))
  (define param (cdr param-mode-pair))

  (cond
    [(= mode 0) param]
    [(= mode 2) (+ param rb)]))

(provide translate-write-pair)

(define (update-program program pos val)
  (if (>= pos (length program))
    ; Take the current program and append a list of 0s until
    (list-set (append program (make-list (- pos (- (length program) 1)) 0)) pos val)
    (list-set program pos val)))

(provide update-program)
