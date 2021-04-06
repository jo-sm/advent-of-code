#lang racket/base

(require racket/list)

(define (play arrangement remaining-rounds)
  (define highest-cup 9)
  (define current-cup (car arrangement))
  (define destination-cup (- current-cup 1))
  (define-values (picked-up remaining) (split-at (cdr arrangement) 3))
  (define new-arrangement (cons current-cup remaining))

  (if (= remaining-rounds 0)
      (append (drop arrangement (- (length arrangement) (index-of arrangement 1)))
              (take arrangement (- (length arrangement) (index-of arrangement 1))))
      (let loop ([destination destination-cup])
        (cond
          [(< destination 1) (loop highest-cup)]
          [(index-of picked-up destination) (loop (- destination 1))]
          [else (let* ([i (index-of new-arrangement destination)]
                       [next (append (take new-arrangement (+ i 1))
                                     picked-up
                                     (drop new-arrangement (+ i 1)))])
                  (play (append (cdr next) (list (car next))) (- remaining-rounds 1)))]))))

(define example (map (λ (n-char) (string->number (string n-char))) (string->list "389125467")))
(define input (map (λ (n-char) (string->number (string n-char))) (string->list "198753462")))

(foldl (λ (n memo) (string-append memo (number->string n))) "" (play input 100))
