#lang racket/base

(require racket/file
         racket/string
         racket/list
         srfi/26
         rackunit)

(define decks
  (let* ([raw (string-split (file->string "input") "\n\n")]
         [decks-strs (map (compose cdr (cut string-split <> "\n")) raw)])
    (map (λ (deck-str)
           (map string->number deck-str))
         decks-strs)))

(define hands
  (let loop ([p1-deck (first decks)] [p2-deck (second decks)])
    (cond
      [(or (empty? p1-deck)
           (empty? p2-deck))
       (list p1-deck p2-deck)]
      [(> (car p1-deck) (car p2-deck))
       (loop (append (cdr p1-deck) (list (car p1-deck) (car p2-deck))) (cdr p2-deck))]
      [else (loop (cdr p1-deck) (append (cdr p2-deck) (list (car p2-deck) (car p1-deck))))])))

(check-equal? (for/list ([hand hands])
                (for/sum ([(card-num i) (in-indexed (reverse hand))]) (* card-num (add1 i))))
              '(0 30197))
