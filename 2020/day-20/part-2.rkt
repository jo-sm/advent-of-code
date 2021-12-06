#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/set
         srfi/26
         racket/trace)

(struct image-tile (id arrangement))

(define (flip-vertical lst)
  (reverse lst))

(define (flip-horizontal lst)
  (map reverse lst))

(define (rotate-cw lst)
  (for/list ([i (range 0 (length lst))])
    (reverse (map (cut list-ref <> i) lst))))

(define (tile-flip-vertical tile)
  (image-tile (image-tile-id tile) (flip-vertical (image-tile-arrangement tile))))

(define (tile-flip-horizontal tile)
  (image-tile (image-tile-id tile) (flip-horizontal (image-tile-arrangement tile))))

(define (tile-rotate-cw tile)
  ; Translate into columns (no additional change needed)
  (image-tile (image-tile-id tile) (rotate-cw (image-tile-arrangement tile))))

(define (tile-rotate-ccw tile)
  ; Translate into columns (no additional change needed)
  (image-tile (image-tile-id tile)
              (for/list ([i (range (- (length (image-tile-arrangement tile)) 1) -1 -1)])
                (map (cut list-ref <> i) (image-tile-arrangement tile)))))

(define (get-possible-arrangements tile)
  (list tile
        (tile-rotate-cw tile)
        (tile-rotate-cw (tile-rotate-cw tile))
        (tile-rotate-cw (tile-rotate-cw (tile-rotate-cw tile)))
        (tile-flip-vertical tile)
        (tile-rotate-cw (tile-flip-vertical tile))
        (tile-rotate-cw (tile-rotate-cw (tile-flip-vertical tile)))
        (tile-rotate-cw (tile-rotate-cw (tile-rotate-cw (tile-flip-vertical tile))))))

(define (get-possible-arrangements-str strs)
  (list strs
        (rotate-cw strs)
        (rotate-cw (rotate-cw strs))
        (rotate-cw (rotate-cw (rotate-cw strs)))
        (flip-vertical strs)
        (rotate-cw (flip-vertical strs))
        (rotate-cw (rotate-cw (flip-vertical strs)))
        (rotate-cw (rotate-cw (rotate-cw (flip-vertical strs))))))

(define (get-right-edge tile)
  (map (cut list-ref <> (- (length (image-tile-arrangement tile)) 1)) (image-tile-arrangement tile)))

(define (get-left-edge tile)
  (map (cut list-ref <> 0) (image-tile-arrangement tile)))

(define (get-top-edge tile)
  (car (image-tile-arrangement tile)))

(define (get-bottom-edge tile)
  (last (image-tile-arrangement tile)))

(define (get-surrounding-tiles image x y)
  (define side-length (sqrt (length image)))

  ; x+1
  (list (and (< side-length (+ x 1))
             (list-ref (list-ref image y) (+ x 1)))
        ; ; y+1
        (and (< side-length (+ y 1))
             (list-ref (list-ref image (+ y 1)) x))
        ; x-1
        (and (>= (- x 1) 0)
             (list-ref (list-ref image y) (- x 1)))
        ; ; y-1
        (and (>= (- y 1) 0)
             (list-ref (list-ref image (- y 1)) x))))

(define tiles
  (let* ([raw-tiles (map (cut string-split <> "\n") (string-split (file->string "input") "\n\n"))]
         [tile-ids (map (lambda (raw) (string->number (substring raw 5 (sub1 (string-length raw)))))
                        (map car raw-tiles))]
         [tiles-strs (map cdr raw-tiles)])
    (for/list ([strs tiles-strs] [id tile-ids])
      (image-tile id
                  (for/list ([str strs])
                    (string->list str))))))

(define (match-edge tiles current-tile tiles-edge-fn current-tile-edge-fn)
  (define current-tile-edge (current-tile-edge-fn current-tile))

  (findf (λ (tile)
           (equal? (tiles-edge-fn tile) current-tile-edge))
    tiles))

(define (get-normal-edges tile)
  (list (get-top-edge tile) (get-right-edge tile) (get-bottom-edge tile) (get-left-edge tile)))

(define (get-all-edges tile)
  (define normal-edges (get-normal-edges tile))
  (define reversed-edges (map reverse normal-edges))

  (append normal-edges reversed-edges))

(define (find-adjacent-tiles tiles current-tile)
  (flatten (for/list ([tile tiles])
             (cond
               [(equal? current-tile tile) empty]
               [(set-empty? (set-intersect (list->set (get-normal-edges current-tile))
                                           (list->set (get-all-edges tile))))
                empty]
               [else tile]))))

; Align `unaligned` in `unaligned-position` of `aligned` tile
(define (align-tile aligned unaligned unaligned-position)
  (define arrangements (get-possible-arrangements unaligned))

  (define (find-alignment arrangement)
    ; (displayln (image-tile-id arrangement))
    ; (displayln (get-left-edge arrangement))
    (case unaligned-position
      [("right") (equal? (get-right-edge aligned) (get-left-edge arrangement))]
      [("below") (equal? (get-bottom-edge aligned) (get-top-edge arrangement))]))

  (define result
    (findf find-alignment
      arrangements))

  ; (get-bottom-edge aligned) (get-top-edge arrangement)

  ; (displayln "\n\n\n\n")
  ; (displayln (list "-> align-tile RESULT " (get-right-edge aligned) (get-left-edge result)))
  ; (displayln (image-tile-arrangement aligned))
  ; (displayln (image-tile-arrangement result))
  ; (displayln result)

  ; (struct-copy image-tile result)
  result)
; (trace align-tile)

(define graph
  (map (λ (tile)
         (list tile (find-adjacent-tiles tiles tile)))
       tiles))
(define start
  (findf (λ (node)
           (= (length (second node)) 2))
    graph))

; Align this node so that the direction of the image will be right and down (easier to build list)
(define (align-start tile neighbors [iter-count 0])
  ; (displayln (image-tile-arrangement tile))
  (define neighbor-position
    (if (= (length neighbors) 2)
      "right"
      "below"))

  (let ([found (align-tile tile (car neighbors) neighbor-position)])
    (cond
      [(and found
            (empty? (cdr neighbors)))
       tile]
      [found (align-start tile (cdr neighbors) 0)]
      [(> iter-count 6) (error "too many tries")]

      ; not found
      [(eq? neighbor-position "right")
       (align-start (tile-flip-horizontal (tile-rotate-cw tile)) neighbors (+ iter-count 1))]
      ; [(eq? neighbor-position "right") (align-start (tile-flip-horizontal (tile-rotate-cw tile)) neighbors 0)]
      [else (align-start (tile-flip-vertical tile) neighbors (+ iter-count 1))])))

; (define tile1171 (car (findf (lambda (node) (= (image-tile-id (first node)) 1171)) graph)))

; (image-tile-arrangement tile1171)
; (image-tile-arrangement (tile-rotate-cw (tile-rotate-cw (tile-rotate-cw (tile-rotate-cw tile1171)))))
; (image-tile-arrangement (tile-rotate-cw tile1171))
; (image-tile-arrangement (tile-rotate-cw (tile-rotate-cw tile1171)))
; (image-tile-arrangement (tile-rotate-cw (tile-rotate-cw (tile-rotate-cw tile1171))))
; (image-tile-arrangement (tile-rotate-ccw tile1171))
; (image-tile-arrangement tile1171)
; (image-tile-arrangement tile1171)

(define aligned-start (align-start (first start) (second start)))

; (displayln (image-tile-arrangement aligned-start)) (flush-output)

; (image-tile-arrangement aligned-start)

(define (get-tile-from-image image x y)
  (list-ref (list-ref image y) x))

(define (remove-node-from-graph graph tile)
  (filter-not (λ (node)
                (= (image-tile-id (first node)) (image-tile-id tile)))
              graph))

(define (get-node-from-graph graph tile)
  (findf (λ (node)
           (= (image-tile-id (first node)) (image-tile-id tile)))
    graph))

(define (add-tile-to-image image tile x y)
  (list-set image y (list-set (list-ref image y) x tile)))

(define (find-tile-in-image image tile)
  ; (displayln (list "find-tile-in-image" tile))
  (define result
    (findf (λ (line)
             (findf (λ (point)
                      (and point
                           (= (image-tile-id point) (image-tile-id tile))))
               line))
      image))

  ; (displayln (list "-> find-tile-in-image: " (image-tile-id tile) (or (and result "FOUND") "not found")))

  result)

(define (build-image graph starting-tile)
  (define image
    (let ([base (make-list (sqrt (length tiles)) (make-list (sqrt (length tiles)) #f))])
      (add-tile-to-image base starting-tile 0 0)))

  (define (iter image graph x y)
    ; (displayln (list "-> ITER " image))
    (define aligned-tile
      (cond
        [(= x 0) (get-tile-from-image image 0 (- y 1))]
        [else (get-tile-from-image image (- x 1) y)]))

    (define unaligned-position
      (if (= x 0)
        "below"
        "right"))

    (define aligned-node (get-node-from-graph graph aligned-tile))
    (define neighbors (second aligned-node))

    ; (displayln (image-tile-arrangement aligned-tile))

    ; (displayln (image-tile-id aligned-tile))
    ; (displayln (map image-tile-id neighbors))
    ; (displayln (map (cut find-tile-in-image image <>) neighbors))

    (define aligned-neighbor (map (cut align-tile aligned-tile <> unaligned-position) neighbors))

    ; (displayln aligned-neighbor)
    (define aligned-new
      (findf (λ (neighbor)
               (and neighbor
                    (not (find-tile-in-image image neighbor))))
        aligned-neighbor))

    ; (displayln aligned-new)

    ; (add-tile-to-image image aligned-new x y)

    (cond
      [(and (= (+ y 1) (length image))
            (= (+ x 1) (length image)))
       (add-tile-to-image image aligned-new x y)]
      ; [(and (= y 1) (= (+ x 1) (length image))) (add-tile-to-image image aligned-new x y)]
      [(= (+ x 1) (length image)) (iter (add-tile-to-image image aligned-new x y) graph 0 (+ y 1))]
      ; [(= (+ x 1) (length image)) (add-tile-to-image image aligned-new x y)]
      [else (iter (add-tile-to-image image aligned-new x y) graph (+ x 1) y)]))

  (iter image graph 1 0))

(define (list-middle lst)
  (cdr (reverse (rest (reverse lst)))))

(define (trim-tile tile)
  ; Get second - second from last rows, and remove first and last chars from each row
  (define trimmed-arrangement (map list-middle (list-middle (image-tile-arrangement tile))))

  (image-tile (image-tile-id tile) trimmed-arrangement))

(define trimmed (map (cut map trim-tile <>) (build-image graph aligned-start)))

(define (find-char-at-positions line positions char shift)
  (= (length (filter (cut eq? #f <>)
                     (map (λ (pos)
                            (char=? (list-ref line (+ pos shift)) char))
                          positions)))
     0))
; (trace find-char-at-positions)

(define (find-num-dragons remaining-lines [cur-shift 0] [result 0])
  (define offsets (list '(18) '(0 5 6 11 12 17 18 19) '(1 4 7 10 13 16)))
  (define cut-find (cut find-char-at-positions <> <> #\# cur-shift))

  (cond
    [(= (length remaining-lines) 2) result]
    [(>= (+ cur-shift 19) (length (first remaining-lines)))
     (find-num-dragons (cdr remaining-lines) 0 result)]
    [(and (cut-find (first remaining-lines) (first offsets))
          (cut-find (second remaining-lines) (second offsets))
          (cut-find (third remaining-lines) (third offsets)))
     (find-num-dragons remaining-lines (+ cur-shift 1) (+ result 1))]
    [else (find-num-dragons remaining-lines (+ cur-shift 1) result)]))
; (trace find-num-dragons)

(define strs
  (for/fold ([result '()])
    ([f trimmed])
    (append result
            (map (λ (p)
                   (foldl (λ (r memo)
                            (append memo r))
                          '()
                          p))
                 (reverse (for/fold ([result '()])
                            ([i (range 0 (length (image-tile-arrangement (car f))))])
                            (cons (map (λ (tile)
                                         (list-ref (image-tile-arrangement tile) i))
                                       f)
                                  result)))))))

; strs

; (find-num-dragons strs)

(define found-arrangement
  (foldl (λ (arng memo)
           (if memo
             memo
             (let ([num (find-num-dragons arng)])
               (and (> num 0)
                    (list num (flatten arng))))))
         #f
         (get-possible-arrangements-str strs)))

(- (count (cut eq? <> #\#) (second found-arrangement)) (* (first found-arrangement) 15))
