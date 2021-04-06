#lang racket

(require "../utils.rkt")
(require rackunit)
(require math/number-theory)
(require srfi/26)

(struct bus (num expected-time))

(define (file-parser file)
  (define parts (string-split file))
  (define schedule (string-split (cadr parts) ","))
  (define bus-numbers (filter number? (map string->number schedule)))

  (define expected-times (map (lambda (n) (index-of schedule (number->string n))) bus-numbers))

  (map bus bus-numbers expected-times))

(define (sieve schedule)
  ; Calculate the highest possible number by multiplying all together
  (define highest (foldl (lambda (i memo) (* (bus-num i) memo)) 1 schedule))

  (define initial (- highest (- (bus-num (car schedule)) (bus-expected-time (car schedule)))))

  (displayln (list highest initial))

  (define (iter remaining cur n iter-count)
    (displayln (list cur
                     n
                     iter-count
                     (bus-num (cadr remaining))
                     (- (bus-num (cadr remaining)) (bus-expected-time (cadr remaining)))))

    (cond
      [(< cur 0) null]
      [(empty? remaining) cur]
      [(and (empty? (cdr remaining))
            (= (modulo (- cur (* n iter-count)) (caar remaining))
               (- (caar remaining) (cadar remaining))))
       (iter (cdr remaining) (- cur (* n iter-count)) (* n (caar remaining)) 1)]
      [(= (modulo (- cur (* n (bus-num (car remaining))) iter-count)) (bus-num (cadr remaining)))
       (- (bus-num (cadr remaining)) (bus-expected-time (cadr remaining)))
       (iter (cdr remaining) (- cur (* n iter-count)) (* n (bus-num (car remaining))) 1)]
      [else (iter remaining (- cur (* n (caar remaining) iter-count)) n (+ iter-count 1))]))

  (iter schedule initial 1 1))

(sieve (read-input-file "example" #:file-parser file-parser))
; (check-eq?
;   (find-first-possible-time (read-input-file "example" #:file-parser file-parser))
;   1068781)

; (find-first-possible-time (read-input-file #:file-parser file-parser))
