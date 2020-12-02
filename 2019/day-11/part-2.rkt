#lang racket

(require "../Intcode/runner.rkt")
(require "../Intcode/constants.rkt")
(require "../utils.rkt")

(define directions
  (list
    (cons 0 -1) ; up
    (cons 1 0) ; right
    (cons 0 1) ; down
    (cons -1 0) ; left
  )
)

(define (get-highest pairs-set proc)
  (foldr (lambda (i memo) (if (> (proc i) (proc memo)) i memo)) (cons 0 0) (set->list pairs-set))
)

(define (get-lowest pairs-set proc)
  (foldr (lambda (i memo) (if (< (proc i) (proc memo)) i memo)) (cons +inf.0 +inf.0) (set->list pairs-set))
)

(define (add-pair a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))
)

(define (new-coords cur-coords cur-dir update)
  (define delta
    (if (= update 0)
      -1
      1
    )
  )

  (define new-dir (modulo (+ cur-dir delta) 4))

  (cons (add-pair cur-coords (list-ref directions new-dir)) new-dir)
)

(define (run-robot runner)
  (define (iter result cur-coords cur-dir white-panels)
    (define input
      ; If current coords are white, give 1, else 0
      (if (set-member? white-panels cur-coords)
        1
        0
      )
    )

    (define prog-run (runner input))
    (define out (drop (first prog-run) (- (length (first prog-run)) 2)))
    (define next-coords-dir (new-coords cur-coords cur-dir (second out)))
    (define new-white-panels
      (if (= (first out) 1)
        (set-add white-panels cur-coords)
        (set-remove white-panels cur-coords)
      )
    )

    (cond
      ((finished? runner) white-panels)
      (else
        (iter
          (set-add result cur-coords)
          (car next-coords-dir)
          (cdr next-coords-dir)
          new-white-panels
        )
      )
    )
  )

  (iter (set) (cons 0 0) 0 (set (cons 0 0)))
)

(define program (flatten (read "Intcode.program" (lambda (line) (map (lambda (i) (string->number i)) (string-split line ","))))))

(define white-panels (run-robot (start program)))

(define y-end (cdr (get-highest white-panels cdr)))
(define y-start (cdr (get-lowest white-panels cdr)))
(define x-end (car (get-highest white-panels car)))
(define x-start (car (get-lowest white-panels car)))

(define y-pixels (range y-start (+ y-end 1)))
(define x-pixels (range x-start (+ x-end 1)))

(define lines (map (lambda (y)
  (map (lambda (x)
    (if (set-member? white-panels (cons x y))
      (get-color "white")
      (get-color "black")
    )
  ) x-pixels)
) y-pixels))

(create-jpg "output.jpg" lines #:scaling-factor 10)
