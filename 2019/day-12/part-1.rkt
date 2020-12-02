#lang racket

(require "../utils.rkt")

(struct body (px py pz vx vy vz))

(define (create-body x y z)
  (body x y z 0 0 0)
)

(define (run init-conds n)
  (define (iter result n rest)
    (cond
      ((= n 0) result)

    )
  )

  (iter null n init-conds)
)
