#lang racket/base

(require racket/string
         racket/file
         rackunit
         racket/trace)

(define (char->number c)
  (string->number (string c)))

(define (run ops)
  (define (parse result remain)
    (cond
      [(null? remain) result]
      [(equal? (car remain) #\()
       (let ([inner-parsed (parse null (cdr remain))])
         (parse (cons (reverse (car inner-parsed)) result) (cadr inner-parsed)))]
      [(equal? (car remain) #\)) (list result (cdr remain))]
      [(char-numeric? (car remain)) (parse (cons (char->number (car remain)) result) (cdr remain))]
      [(equal? (car remain) #\space) (parse result (cdr remain))]
      [else (parse (cons (car remain) result) (cdr remain))]))

  (define (calculate lst)
    (define (calc-add intermediate remain)
      (cond
        [(null? remain) intermediate]

        [(and (= (length intermediate) 0) (list? (car remain)))
         (calc-add (cons (calculate (car remain)) intermediate) (cdr remain))]
        [(= (length intermediate) 0) (calc-add (cons (car remain) intermediate) (cdr remain))]
        [(and (equal? (car remain) #\+) (list? (cadr remain)))
         (calc-add (cons (+ (car intermediate) (calculate (cadr remain))) (cdr intermediate))
                   (cddr remain))]
        [(equal? (car remain) #\+)
         (calc-add (cons (+ (car intermediate) (cadr remain)) (cdr intermediate)) (cddr remain))]
        [(list? (cadr remain)) (calc-add (cons (calculate (cadr remain)) intermediate) (cddr remain))]
        [else (calc-add (cons (cadr remain) intermediate) (cddr remain))]))

    (apply * (calc-add null lst)))

  (calculate (reverse (parse null ops))))

(check-eq? (run (string->list "1 + 2 * 3 + 4 * 5 + 6")) 231)
(check-eq? (run (string->list "1 + (2 * 3) + (4 * (5 + 6))")) 51)
(check-eq? (run (string->list "2 * 3 + (4 * 5)")) 46)
(check-eq? (run (string->list "5 + (8 * 3 + 9 + 3 * 4 * 3)")) 1445)
(check-eq? (run (string->list "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) 669060)
(check-eq? (run (string->list "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) 23340)

(check-eq? (apply + (map (compose run string->list) (file->lines "input"))) 88782789402798)
