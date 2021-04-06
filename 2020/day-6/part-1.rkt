#lang racket

(require "../utils.rkt")
(require srfi/26)

(define groups-answers
  (map string-split (read-input-file #:file-parser (cut string-split <> "\n\n"))))

(define concatenated-answers (map (cut foldl string-append "" <>) groups-answers))

; Transform to a set of individual characters to ignore any duplicates
; and then count it, effectively counting the unique answers, then add them all up
(apply + (map (compose set-count list->set string->list) concatenated-answers))
