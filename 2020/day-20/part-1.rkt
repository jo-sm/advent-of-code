#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/set
         srfi/26)

(struct image-tile (id arrangement))

(define (tile-flip-vertical tile)
  (image-tile (image-tile-id tile) (reverse (image-tile-arrangement tile))))

(define (tile-flip-horizontal tile)
  (image-tile (image-tile-id tile) (map reverse (image-tile-arrangement tile))))

(define (tile-rotate-cw tile)
  ; Translate into columns (no additional change needed)
  (image-tile (image-tile-id tile)
              (for/list ([i (range 0 (length (image-tile-arrangement tile)))])
                (map (cut list-ref <> i) (image-tile-arrangement tile)))))

(define (tile-rotate-ccw tile)
  ; Translate into columns (no additional change needed)
  (image-tile (image-tile-id tile)
              (for/list ([i (range (- (length (image-tile-arrangement tile)) 1) -1 -1)])
                (map (cut list-ref <> i) (image-tile-arrangement tile)))))

(define (get-possible-arrangements tile)
  (list tile
        (tile-rotate-cw tile)
        (tile-rotate-cw (tile-rotate-cw tile))
        (tile-rotate-ccw tile)
        (tile-flip-vertical tile)
        (tile-rotate-cw (tile-flip-vertical tile))
        (tile-rotate-cw (tile-rotate-cw (tile-flip-vertical tile)))
        (tile-rotate-ccw (tile-flip-vertical tile))))

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
                    (map (lambda (c)
                           (if (char=? c #\#)
                             1
                             0))
                         (string->list str)))))))

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

(apply *
       (map image-tile-id
            (filter (λ (tile)
                      (= (length (find-adjacent-tiles tiles tile)) 2))
                    tiles)))
