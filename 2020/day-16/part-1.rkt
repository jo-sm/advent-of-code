#lang racket/base

(require racket/file
         racket/list
         racket/string
         srfi/26)

(struct section (name min1 max1 min2 max2) #:transparent)

(define (parse-file file)
  (define-values (sections-raw ticket-raw other-tickets-raw)
    (apply values (string-split file "\n\n")))

  (define (parse-section section-raw)
    (define pieces
      (rest (regexp-match #px"^([[a-z ]+): ([0-9]{1,4})-([0-9]{1,4}) or ([0-9]{1,4})-([0-9]{1,4})$"
                          section-raw)))

    (section (first pieces)
             (string->number (second pieces))
             (string->number (third pieces))
             (string->number (fourth pieces))
             (string->number (fifth pieces))))

  (define sections (map parse-section (string-split sections-raw "\n")))
  (define my-ticket
    ((compose (cut map string->number <>) (cut string-split <> ","))
     (second (string-split ticket-raw "\n"))))
  (define other-tickets
    (map (cut map string->number <>)
         ((cut map (lambda (i) (string-split i ",")) <>)
          (rest (string-split other-tickets-raw "\n")))))

  (list sections my-ticket other-tickets))

(define (validate-ticket sections ticket)
  (for/fold ([invalid null] [remaining-sections sections] #:result invalid)
    ([ticket-num ticket])
    (let ([found-section (findf (lambda (section)
                                  (or (<= (section-min1 section) ticket-num (section-max1 section))
                                      (<= (section-min2 section) ticket-num (section-max2 section))))
                           remaining-sections)])

      (if found-section
        (values invalid remaining-sections)
        (values (cons ticket-num invalid) remaining-sections)))))

(define example-raw (file->string "example"))
(define example (parse-file example-raw))
(define input-raw (file->string "input"))
(define input (parse-file input-raw))

(apply + (flatten (map (lambda (ticket) (validate-ticket (first example) ticket)) (third example))))

(apply + (flatten (map (lambda (ticket) (validate-ticket (first input) ticket)) (third input))))
