#lang racket/base

(require racket/file
         racket/string
         racket/set
         racket/list
         rackunit)

(define ingredients
  (for/list ([line (file->lines "input")])
    (let ([ingredients (string-split (car (string-split line " (contains ")))]
          [allergens (string-split (cadr (string-split (substring line 0 (- (string-length line) 1))
                                                       " (contains "))
                                   ", ")])
      (list (list->set ingredients) (list->set allergens)))))

(define all-allergen-names
  (foldl (λ (ingredient memo) (set-union memo (second ingredient))) (set) ingredients))

(define all-allergens
  (for/fold ([allergens (set)]) ([allergen (in-set all-allergen-names)])
    (set-union
     allergens
     (foldl (λ (ingredient memo)
              (if (set-member? (second ingredient) allergen)
                  (if (set-empty? memo) (first ingredient) (set-intersect memo (first ingredient)))
                  memo))
            (set)
            ingredients))))

(check-eq? (length (for/fold ([result '()]) ([ingredient ingredients])
                     (append result (set->list (set-subtract (first ingredient) all-allergens)))))
           2517)
