#lang racket

(require "../utils.rkt")
(require srfi/26)

(struct euclidean-vector (direction magnitude) #:transparent)
(define (construct-euclidean-vector vector-string)
  (euclidean-vector (substring vector-string 0 1) (string->number (substring vector-string 1))))

(struct point (x y) #:transparent)

(define (rectilinear-distance point)
  ; Since we are always measuring the distance from 0,0 we can simply add the absolute values together
  ; The general case: |p1-q1|+|p2-q2|
  (+ (abs (point-x point)) (abs (point-y point))))

(define (generate-points last-point vector)
  ; Create a range of next x or y coordinates for the generated points
  (define next-coordinates
    (case (euclidean-vector-direction vector)
      [("U") (range (+ (point-y last-point) 1)
                    (+ (point-y last-point) (euclidean-vector-magnitude vector) 1))]
      [("D") (range (- (point-y last-point) 1)
                    (- (point-y last-point) (euclidean-vector-magnitude vector) 1)
                    -1)]
      [("R") (range (+ (point-x last-point) 1)
                    (+ (point-x last-point) (euclidean-vector-magnitude vector) 1))]
      [("L") (range (- (point-x last-point) 1)
                    (- (point-x last-point) (euclidean-vector-magnitude vector) 1)
                    -1)]))

  (case (euclidean-vector-direction vector)
    [("U" "D") (map (lambda (coordinate) (point (point-x last-point) coordinate)) next-coordinates)]
    [("R" "L") (map (lambda (coordinate) (point coordinate (point-y last-point))) next-coordinates)]))

(define (find-intersections path-a path-b)
  (define intersections (set-intersect (list->set path-a) (list->set path-b)))

  ; 0,0 isn't really an intersection since both paths start from there
  ; due to the way the paths are constructed
  (set->list (set-remove intersections (point 0 0))))

(define vectors-lists
  (map (cut map construct-euclidean-vector <>) (read "input" (cut string-split <> ","))))

(define paths
  (map (cut foldl
            (lambda (current-vector points)
              (append points (generate-points (last points) current-vector)))
            (list (point 0 0))
            <>)
       vectors-lists))

(define intersections (apply find-intersections paths))

(first (sort (map rectilinear-distance intersections) <))
