#lang racket

(require plot)
(define depth 10000)
(define delta-e 0.01)

;; Rossler Attractor
;; '(x y z) -> '(x+dx, y+dy, z+dz)
(define (rossler l)
  (let* ([a 0.2]
         [b 0.2]
         [c 6.7]
         [dx (λ (x y z)
                    (+ x (* delta-e (+ (- y) (- z)))))]
         [dy (λ (x y z)
                    (+ y (* delta-e (+ x (* b y)))))]
         [dz (λ (x y z)
                   (+ z (* delta-e (+ a (* z (- x c))))))])
    (list (dx (first l) (second l) (third l))
          (dy (first l) (second l) (third l))
          (dz (first l) (second l) (third l)))))


;; Lorenz attractor equations
;; '(x y z) -> '(x+dx, y+dy, z+dz)
(define (lorenz l)
  (let* ([s 10]
         [r 28]
         [b 8/3]
         [dx (λ (x y z)
                      (+ x (* delta-e (* s (- y x)))))]
         [dy (λ (x y z)
                      (+ y (* delta-e (- (* r x) y (* x z)))))]
         [dz (λ (x y z)
                      (+ z (* delta-e (- (* x y) (* b z)))))])
    (list (dx (first l) (second l) (third l))
          (dy (first l) (second l) (third l))
          (dz (first l) (second l) (third l)))))


;; Function Point -> List-of-points
;; Creates a list of points based on the given function and starting point
(define (pt-list f start)
  (define (iter result count)
    (if (<= count 0)
        (cons (f (first result)) result)
        (iter (cons (f (first result)) result) (sub1 count))))
  (iter (cons start '()) depth))

;; Generates plots for the rossler and lorenz attractors
(plot3d (points3d (pt-list rossler '(1 0 0)) #:sym 'dot)
          #:altitude 25 #:title "Rossler")

(plot3d (points3d (pt-list lorenz '(1 0 0)) #:sym 'dot)
          #:altitude 25 #:title "Lorenz")
