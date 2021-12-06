#lang racket/base

(require racket/file
         racket/function
         racket/string
         racket/trace
         racket/list
         srfi/26
         memoize)

(define (memoize fn)
  (let ([cache (make-hash)]) (lambda arg (hash-ref! cache arg (thunk (apply fn arg))))))

(define (char->number c)
  (string->number (string c)))

(define (_and fns)
  (define (and-inner str)
    ; (displayln (list "CALL and-inner: " str fns))
    (for/fold ([passed #t] [remaining str]
                           #:result (and passed
                                         remaining))
      ([fn fns])
      ; (displayln (list "-> (and) fn" remaining passed fn))
      (if (not passed)
        (values passed remaining)
        (let ([next (fn remaining)])
          (if next
            (values #t next)
            (values #f ""))))))

  and-inner)

(define (_or fns)
  (define (or-inner str)
    ; (displayln (list "CALL or-inner: " str fns))
    (for/fold ([result str] [passed #f]
                            #:result (and passed
                                          result))
      ([fn fns])
      ; (displayln (list "-> (or) fn" result passed fn))
      (if passed
        (values result passed)
        (let ([next (fn result)])
          (if next
            (values next #t)
            (values result passed))))))

  or-inner)

(define (parse-and-rule rules nums)
  (define parsed (map (lambda (n) (parse-rule rules n)) nums))

  ; (displayln (list "-> parse-and-rule " nums parsed))

  (define to-be-mapped
    (if (list? (car parsed))
      parsed
      (list parsed)))

  (map _and to-be-mapped)

  ; parsed
  )

(define/memo (startswith piece)
             (define (startswith-inner s)
               ; (displayln (list "-> startswith-inner: " s piece))
               (and (string-prefix? s piece)
                    (substring s (string-length piece))))
             startswith-inner)

(define/memo (parse-rule rules rule)
             (_or (for/list ([subrule rule])
                    (if (string? subrule)
                      (lambda (s)
                        (and (string-prefix? s subrule)
                             (substring s (string-length subrule))))
                      (_and (map (lambda (n) (parse-rule rules (list-ref rules n))) subrule))))))

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
            (for/fold ([result (make-list (length raw-rules) empty)])
              ([rule raw-rules])
              (list-set result
                        (car rule)
                        (append (for/list ([pieces (cadr rule)])
                                  (for/fold ([result '()])
                                    ([piece pieces] #:when (or (string->number piece)
                                                               (regexp-match? "\"[a-z]\"" piece)))
                                    (let ([possible-num (string->number piece)])
                                      (if possible-num
                                        (append result (list possible-num))
                                        (substring piece 1 2) ;always a single letter
                                        ))))))))))

; (define-values (messages example) (parse "example"))
(define-values (messages input) (parse "input"))

; (displayln example)

(define rule
  (lambda (str)
    (begin
      ; (displayln (list "testing: " str)) (flush-output)
      ((parse-rule input (list-ref input 0)) str))))

(count (cut equal? <> "") (map rule messages))

; (map (cut parse-rule example <>) )
