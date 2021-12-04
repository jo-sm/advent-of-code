#lang racket/base

(require "./utils.rkt"
         threading
         racket/list
         racket/string
         racket/match
         rackunit)

#| Day 4: Giant Squid

The first tricky day! The trickiness was a combination of misreading the problem (or rather, skimming), figuring out how to nicely parse
the input, and doing this in the morning.

For parsing, the way I did it required the use of a map on each list of lists, which I call a `mapmap`. I wonder if this is another area
where using `qi` would have made it simpler (as you could use `><`).

For skimming, the problem mentioned bingo and so I went on the assumption I need to check the columns, rows, and diagonals. This turned out
not to be true - in this problem only the rows and columns matter. Only needing to transpose made it simpler at least.

I also made my life hard by first thinking I could take the most recent 5 "drawn" numbers (I think I forgot how bingo works 🤔) and then
thinking that I could make combinations of 5 numbers of all drawn numbers. This wouldn't work because it's still order dependent, and to
get this approach to work I would need to do combinations -> permutations on each combo. I worried that this would be too slow for part 2
and scrapped the idea, instead going with an approach using counting and filtering.

In part 2 I got quite frustrated as well when my function returned the right value for the example but not the input. As should be expected
the example is made in such a way that it accepts slightly invalid solutions - in my case, once removing the second board, the next number (13)
is an actual solution, but this isn't generally true, and so after a lot of head scratching I realized I need to actually make sure that there
was a row/column that had a match on the board before calculating the score.
|#

; Assumes a neat n⨉n list
(define (transpose matrix)
  (for/list ([i (range (length matrix))])
    (map (λ (row) (list-ref row i)) matrix)))

(define (mapmap pred lists)
  (map (λ (lst) (map pred lst)) lists))

(define (chunk lst n)
  (let loop ([remaining lst] [result '()])
    (cond
      [(null? remaining) (reverse result)]
      [(> n (length remaining)) (loop '() (cons remaining result))]
      [else (loop (drop remaining n) (cons (take remaining n) result))])))

(define (get-first-winning-row board-config nums len)
  (define nums-in-rows (map (λ (row) (filter (λ (n) (member n row)) nums)) board-config))

  (findf (λ (n) (= len (length n))) nums-in-rows))

(define (has-winning-row? board nums)
  (or (get-first-winning-row board nums 5) (get-first-winning-row (transpose board) nums 5)))

(define (calculate-score board nums)
  (~>> board flatten (remove* nums) (apply +) (* (last nums))))

(define (part-1 input)
  (match-define (list nums-to-draw boards) input)

  (let search ([n 5])
    (define drawn-nums (take nums-to-draw n))
    (define winning-board (findf (λ (board) (has-winning-row? board drawn-nums)) boards))

    (if winning-board (calculate-score winning-board drawn-nums) (search (add1 n)))))

(define (part-2 input)
  (match-define (list nums-to-draw boards) input)

  (let search ([remaining-boards boards] [n 5])
    (define drawn-nums (take nums-to-draw n))
    (define winning-boards (filter (λ (board) (has-winning-row? board drawn-nums)) remaining-boards))

    (cond
      [(and (= (length remaining-boards) 1) (not (null? winning-boards)))
       (calculate-score (car remaining-boards) drawn-nums)]
      [(not (null? winning-boards)) (search (remove* winning-boards remaining-boards) (add1 n))]
      [else (search remaining-boards (add1 n))])))

(define (parser raw)
  (define lines (string-split raw "\n"))
  (define nums-to-draw (~> (car lines) (string-split ",") (map string->number _)))
  (define boards
    (~> (cdr lines)
        (chunk 6)
        (map cdr _)
        (mapmap (λ (line) (~> line string-split (map string->number _))) _)))

  (list nums-to-draw boards))

(define example
  (~>
   "
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
8  2 23  4 24
21  9 14 16  7
6 10  3 18  5
1 12 20 15 19

3 15  0  2 22
9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
2  0 12  3  7"
   ; trim the string so the example above looks a little nicer (basically remove the first line)
   string-trim
   parser))
(define input (read-input-file "04.rktd" #:file-parser parser))

(check-eq? (part-1 example) 4512)
(check-eq? (part-1 input) 16716)

(check-eq? (part-2 example) 1924)
(check-eq? (part-2 input) 4880)
