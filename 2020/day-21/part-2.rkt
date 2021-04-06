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
  (for/list ([allergen (in-set all-allergen-names)])
    (cons allergen
          (foldl
           (λ (ingredient memo)
             (if (set-member? (second ingredient) allergen)
                 (if (set-empty? memo) (first ingredient) (set-intersect memo (first ingredient)))
                 memo))
           (set)
           ingredients))))

(define (reduce-allergens all-allergens)
  (define (iter remaining-allergens result)
    (if (empty? remaining-allergens)
        result
        (let*-values
            ([(singles others) (partition (λ (a) (= (set-count (cdr a)) 1)) remaining-allergens)]
             [(removed)
              (map (λ (other)
                     (foldl (λ (single memo)
                              (cons (car memo) (set-remove (cdr memo) (set-first (cdr single)))))
                            other
                            singles))
                   others)])
          (iter removed (append result singles)))))

  (iter all-allergens '()))

(define alphabetical-allergens
  (foldl (λ (a memo) (append memo (list (set-first (cdr a)))))
         '()
         (sort (reduce-allergens all-allergens) (λ (a b) (string<? (car a) (car b))))))

(check-equal? (string-join alphabetical-allergens ",") "rhvbn,mmcpg,kjf,fvk,lbmt,jgtb,hcbdb,zrb")
