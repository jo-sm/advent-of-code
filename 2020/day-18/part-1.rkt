#lang racket/base

(require racket/string
         racket/file
         rackunit)

(define (char->number c)
  (string->number (string c)))

(define (run ops)
  (define (parse-parentheses result remain)
    (cond
      [(null? remain) result]
      [(equal? (car remain) #\()
       (let ([inner-parsed (parse-parentheses null (cdr remain))])
         (parse-parentheses (append result (list (car inner-parsed))) (cadr inner-parsed)))]
      [(equal? (car remain) #\)) (list result (cdr remain))]
      [(char-numeric? (car remain))
       (parse-parentheses (append result (list (char->number (car remain)))) (cdr remain))]
      [(equal? (car remain) #\space) (parse-parentheses result (cdr remain))]
      [else (parse-parentheses (append result (list (car remain))) (cdr remain))]))

  (define (calculate result remain)
    (cond
      [(null? remain) result]
      [(and (= 0 result)
            (list? (car remain)))
       (calculate (calculate 0 (car remain)) (cdr remain))]
      [(= 0 result) (calculate (car remain) (cdr remain))]
      [(and (equal? (car remain) #\+)
            (list? (cadr remain)))
       (calculate (+ result (calculate 0 (cadr remain))) (cdr remain))]
      [(and (equal? (car remain) #\*)
            (list? (cadr remain)))
       (calculate (* result (calculate 0 (cadr remain))) (cdr remain))]
      [(equal? (car remain) #\+) (calculate (+ result (cadr remain)) (cdr remain))]
      [(equal? (car remain) #\*) (calculate (* result (cadr remain)) (cdr remain))]
      [else (calculate result (cdr remain))]))

  (calculate 0 (parse-parentheses null ops)))

(check-eq? (run (string->list "1 + 2 * 3 + 4 * 5 + 6")) 71)
(check-eq? (run (string->list "1 + (2 * 3) + (4 * (5 + 6))")) 51)
(check-eq? (run (string->list "2 * 3 + (4 * 5)")) 26)
(check-eq? (run (string->list "5 + (8 * 3 + 9 + 3 * 4 * 3)")) 437)
(check-eq? (run (string->list "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) 12240)
(check-eq? (run (string->list "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) 13632)

(check-eq? (apply + (map (compose run string->list) (file->lines "input"))) 5374004645253)
