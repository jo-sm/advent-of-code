#lang racket/base

(require "../utils.rkt"
         threading
         racket/string
         racket/set
         racket/match
         racket/list
         rackunit)

(struct xy (x y) #:transparent)

(define (get-adjacent all coord)
  (define x (xy-x coord))
  (define y (xy-y coord))
  (define length-y (length all))
  (define length-x (length (car all)))

  (define adjacent-y
    (cond
      [(= y 0) '(1)]
      [(= y (sub1 length-y)) (list (- length-y 2))]
      [else (list (add1 y) (sub1 y))]))
  (define adjacent-x
    (cond
      [(= x 0) '(1)]
      [(= x (sub1 length-x)) (list (- length-x 2))]
      [else (list (add1 x) (sub1 x))]))

  (append (map (lambda (_x) (xy _x y)) adjacent-x) (map (lambda (_y) (xy x _y)) adjacent-y)))

(define (get-low-points input)
  (define length-y (length input))
  (define length-x (length (car input)))

  (for/fold ([points '()])
    ([y (in-range length-y)])
    (for/fold ([interm points])
      ([x (in-range length-x)])
      (define curr (matrix-ref input x y))
      (define adjacent
        (~>> (get-adjacent input (xy x y))
             (map (λ (coord)
                    (matrix-ref input (xy-x coord) (xy-y coord))))
             (sort _ <)))

      (if (< curr (car adjacent))
        (cons (xy x y) interm)
        interm))))

(define (get-basin-points-for-coord matrix coord [visited '()])
  (define x (xy-x coord))
  (define y (xy-y coord))

  (define adjacent (get-adjacent matrix coord))

  ; (displayln coord)
  ; (displayln adjacent)
  ; (displayln (= (matrix-ref matrix x y) 9))
  ; (displayln (member coord visited))
  ; (displayln visited)
  ; (displayln "")

  (cond
    [(= (matrix-ref matrix x y) 9) '()]
    [(member coord visited) '()]
    [else (foldl (λ (adj acc)
                   (get-basin-points-for-coord matrix adj (cons coord visited)))
                 '()
                 adjacent)]))

(define (part-1 input)
  (~>> (get-low-points input)
       (map (λ (coord)
              (matrix-ref input (xy-x coord) (xy-y coord))))
       (map add1)
       (apply +)))

#;(define (part-2 input)
    (~>> (map (lambda (coord) (get-basin-points-for-coord input coord)) (get-low-points input))
         ; (map flatten)
         ; (map list->set)
         ; (map set-count)
         ; (map add1)
         ; (sort _ >)
         ; (take _ 3)
         ; (apply *)
         ))

(define (parser line)
  (~>> line
       string->list
       (map char->number)))

(define example (map parser '("2199943210" "3987894921" "9856789892" "8767896789" "9899965678")))
(define input (parse "09.rktd" #t #:parser parser))

(check-eq? (part-1 example) 15)
(check-eq? (part-1 input) 560)

; (check-eq? (part-2 example) 1134)
; (part-2 input)

; (fourth (get-low-points input))
(get-basin-points-for-coord input (fourth (get-low-points input)))
