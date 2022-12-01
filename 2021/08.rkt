#lang racket/base

(require "../utils.rkt"
         threading
         racket/list
         racket/string
         rackunit)

(define (parser line)
  (~> line
      (string-split " | ")
      (map string-split _)))

(define (part-1 input)
  (~>> input
       (map cadr)
       (mapmap (λ (segment)
                 ; corresponding to 1 4 7 8 in order
                 (true? (member (string-length segment) '(2 4 3 7)))))
       flatten
       (filter true?)
       length))

#;(define (part-2 input)
    (define all-possible-configs
      (~> "abcdefg"
          string->list
          permutations)))

(define example
  (~> "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
      string-trim
      (string-split "\n")
      (map parser _)))
(define input (parse "08.rktd" #t #:parser parser))

(check-eq? (part-1 example) 26)
(check-eq? (part-1 input) 261)
