#lang racket

(require "../utils.rkt")
(require srfi/26)

(define (all-lists-contain v lsts)
  (=
    (length lsts)
    (length (filter truthy? (map (cut index-of <> v) lsts))))
)

(define (answers-in-all initial-answer . other-answers)
  (define initial-answer-chars (string->list initial-answer))
  (define other-answers-chars (map string->list other-answers))

  (if (empty? other-answers)
    ; If you're alone, all of your answers are in all of the "group's" answers
    (string-length initial-answer)
    (length
      (filter truthy?
        (map
          (cut all-lists-contain <> other-answers-chars)
          initial-answer-chars))))
)

(define groups-answers
  (map
    string-split
    (read-input-file #:file-parser (cut string-split <> "\n\n"))))

(apply +
  (map
    (cut apply answers-in-all <>)
    groups-answers))
