#lang racket/base

; Trying https://www.reddit.com/r/adventofcode/comments/kf8mlu/2020_day_16_part_3_a_different_number_system/

(require racket/file
         racket/list
         racket/string
         srfi/26
         racket/trace)

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

(define (find-sections sections num)
  (filter (lambda (section)
            (or (<= (section-min1 section) num (section-max1 section))
                (<= (section-min2 section) num (section-max2 section))))
          sections))

(define (validate-ticket sections ticket)
  (for/fold ([invalid #false])
    ([ticket-num ticket])
    (let ([found-sections (find-sections sections ticket-num)])
      ; I SPENT A LOT OF TIME ON THIS LINE
      (if (empty? found-sections)
        #t
        invalid))))

(define (sort-sections sections tickets)
  (define (iter remaining-sections sorted-sections remaining-nums)
    (let* ([next-num (if (empty? remaining-nums)
                       0
                       (car remaining-nums))]
           [nums (map (cut list-ref <> next-num) tickets)]
           [sections
            (foldl (lambda (num sections) (find-sections sections num)) remaining-sections nums)])
      ; (displayln next-num)
      (cond
        [(empty? remaining-nums) sorted-sections]
        [(= (length sections) 1) (iter (remove (first sections) remaining-sections)
                                       (cons (first sections) sorted-sections)
                                       (cdr remaining-nums))]
        [else
         (iter remaining-sections sorted-sections (append (cdr remaining-nums) (list next-num)))])))

  (reverse (iter sections '() (range 0 (length (first tickets))))))

(define (find-valid-sections sections columns)
  (for/fold ([possible-sections '()])
    ([column columns])
    (let (; [i (index-of columns column)]
          [_sections (foldl (lambda (num s) (find-sections s num)) sections column)])
      ; struct-copy is important here
      (append possible-sections (list (map (cut struct-copy section <>) _sections))))))

(define (reduce-sections possible-sections)
  (define (iter result remaining-possible-sections)
    (if (empty? (flatten remaining-possible-sections))
      result
      (let* ([first-single-i (index-where remaining-possible-sections
                                          (lambda (s-list) (= (length s-list) 1)))]
             [first-single (first (list-ref remaining-possible-sections first-single-i))]
             [new-remaining
              (map (lambda (s-list)
                     (remf (lambda (sec) (eq? (section-name sec) (section-name first-single)))
                           s-list))
                   remaining-possible-sections)])
        (iter (hash-set result first-single-i first-single) new-remaining)))
    (trace iter))

  (define result '())

  (hash-map (iter (hash) possible-sections) (lambda (k v) (append result v)) #t))

; (define example-raw (file->string "example2"))
; (define example (parse-file example-raw))
(define input-raw (file->string "input2"))
(define input (parse-file input-raw))

(define valid-tickets
  (cons (second input)
        (filter-not (lambda (ticket) (validate-ticket (first input) ticket)) (third input))))

(define columns
  (map (lambda (i) (map (lambda (ticket) (list-ref ticket i)) valid-tickets))
       (range 0 (length (first valid-tickets)))))

; (displayln columns)

(define ordered-sections (find-valid-sections (first input) columns))

(displayln ordered-sections)
; (apply * (map (cut list-ref (second input) <>) (indexes-where
;   ordered-sections (lambda (section) (string-prefix? (section-name section) "departure")))))
