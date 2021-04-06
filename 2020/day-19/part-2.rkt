#lang racket/base

(require racket/file
         racket/function
         racket/string
         racket/trace
         racket/list
         srfi/26)

(define (_and fns)
  (define (and-inner str)
    ; (displayln (list "CALL and-inner: " str fns)) (flush-output)
    (for/fold ([passed #t] [remaining str] #:result (and passed remaining)) ([fn fns])
      (if (not passed)
          (values passed remaining)
          ; thunked version
          (let ([next ((fn) remaining)])
            (cond
              [(empty? next) (values #f remaining)]
              [else (values #t next)])))))

  and-inner)

(define (_or fns)
  (define (or-inner str)
    ; (displayln (list "CALL or-inner: " str fns)) (flush-output)
    (for/fold ([result null]) ([fn fns])
      ; this bug!! (using result, not str)
      (let ([next (fn str)])
        ; (displayln next)
        (cond
          [(empty? next) result]
          [next (append result next)]
          [else result])
        ; (displayln (list "-> (or)  fn" result passed fn)) (flush-output)
        )))

  or-inner)

(define (parse-and-rule rules nums)
  (define parsed (map (lambda (n) (parse-rule rules n)) nums))

  (define to-be-mapped (if (list? (car parsed)) parsed (list parsed)))

  (map _and to-be-mapped))

(define (startswith piece)
  (define (startswith-inner strs)
    ; (displayln (list "-> (sw):   " s piece))
    (map (lambda (s) (and (string-prefix? s piece) (substring s (string-length piece))))
         (filter-not (cut eq? #f <>) (flatten strs))))

  startswith-inner)

(define (parse-rule rules rule)
  (_or
   (for/list ([subrule rule])
     (if (string? subrule)
         (startswith subrule)
         ; (lambda (s) (displayln (list "startswith: " s)) (and (string-prefix? s subrule) (substring s (string-length subrule))))
         (_and (map (lambda (n)
                      (let ([next-rule (list-ref rules n)]) (thunk* (parse-rule rules next-rule))))
                    subrule))))))

(define (parse filename)
  (let* ([raw (string-split (file->string filename) "\n\n")]
         [raw-rules-1 (map (lambda (rule) (string-split rule ": "))
                           (string-split (list-ref raw 0) "\n"))]
         [raw-rules (map (lambda (r)
                           (list (string->number (car r))
                                 (map string-split (string-split (cadr r) " | "))))
                         raw-rules-1)]
         [messages (string-split (list-ref raw 1))])
    (values messages
            (for/fold ([result (make-list (length raw-rules) empty)]) ([rule raw-rules])
              (list-set
               result
               (car rule)
               (for/list ([pieces (cadr rule)])
                 (for/fold ([result '()])
                           ([piece pieces]
                            #:when (or (string->number piece) (regexp-match? "\"[a-z]\"" piece)))
                   (let ([possible-num (string->number piece)])
                     (if possible-num
                         (append result (list possible-num))
                         (substring piece 1 2) ;always a single letter
                         )))))))))

; (define-values (messages example) (parse "example"))
(define-values (messages input) (parse "input2"))

; (displayln example)

(define rule
  (lambda (str)
    (begin
      ; (displayln (list "testing:   " str)) (flush-output)
      (flatten ((parse-rule input (list-ref input 0)) (list str))))))

; input

(count (lambda (lst) (findf (lambda (i) (equal? i "")) lst)) (map rule messages))

; (rule "ababbb")

; (count (cut equal? "" <>) (map rule messages))

; (map (cut parse-rule example <>) )
