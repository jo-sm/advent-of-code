#lang racket

(length
 (filter odd?
         (for/fold ([numbers '()]) ([combo (in-combinations '(1 2 3 4 5))] #:unless (empty? combo))
           (append numbers
                   (for/list ([perm (in-permutations combo)])
                     (string->number
                      (foldl (λ (n memo) (string-append memo (number->string n))) "" perm)))))))
