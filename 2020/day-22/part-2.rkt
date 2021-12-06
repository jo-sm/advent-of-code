#lang racket/base

(require racket/file
         racket/string
         racket/set
         racket/list
         srfi/26
         rackunit)

(define hands
  (let* ([raw (string-split (file->string "input") "\n\n")]
         [hands-strs (map (compose cdr (cut string-split <> "\n")) raw)])
    (map (λ (hand-str)
           (map string->number hand-str))
         hands-strs)))

(define (play-game hand1 hand2)
  (let loop ([p1-hand hand1] [p2-hand hand2] [p1-prev-hands (set)] [p2-prev-hands (set)])
    (cond
      [(or (empty? p1-hand)
           (empty? p2-hand))
       (list p1-hand p2-hand)]

      ; If we would repeat this configuration, player1 wins immediately
      [(and (set-member? p1-prev-hands p1-hand)
            (set-member? p2-prev-hands p2-hand))
       (list p1-hand '())]

      [(and (> (length p1-hand) (car p1-hand))
            (> (length p2-hand) (car p2-hand)))
       (let ([recursed (play-game (take (cdr p1-hand) (car p1-hand))
                                  (take (cdr p2-hand) (car p2-hand)))])
         (if (empty? (first recursed))
           (loop (cdr p1-hand)
                 (append (cdr p2-hand) (list (car p2-hand) (car p1-hand)))
                 (set-add p1-prev-hands p1-hand)
                 (set-add p2-prev-hands p2-hand))
           (loop (append (cdr p1-hand) (list (car p1-hand) (car p2-hand)))
                 (cdr p2-hand)
                 (set-add p1-prev-hands p1-hand)
                 (set-add p2-prev-hands p2-hand))))]

      [(> (car p1-hand) (car p2-hand)) (loop (append (cdr p1-hand) (list (car p1-hand) (car p2-hand)))
                                             (cdr p2-hand)
                                             (set-add p1-prev-hands p1-hand)
                                             (set-add p2-prev-hands p2-hand))]
      [else (loop (cdr p1-hand)
                  (append (cdr p2-hand) (list (car p2-hand) (car p1-hand)))
                  (set-add p1-prev-hands p1-hand)
                  (set-add p2-prev-hands p2-hand))])))

(define final-hands (apply play-game hands))

(check-equal? (for/list ([hand final-hands])
                (for/sum ([(card-num i) (in-indexed (reverse hand))]) (* card-num (add1 i))))
              '(0 34031))
