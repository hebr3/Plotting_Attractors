#lang typed/racket

(require plot)
(define depth 10000)
(define delta-e 0.01)

;; Rossler Attractor
;; '(x y z) -> '(x+dx, y+dy, z+dz)
;; dx = -y-z
;; dy = x+by
;; dz = a+z(x-c)
(: rossler (-> (Listof Real) (Listof Real)))
(define (rossler l)
  (let* ([x (first l)]
         [y (second l)]
         [z (third l)]
         [a 0.2]
         [b 0.2]
         [c 6.7]
         [dx (λ ([x : Real] [y : Real] [z : Real])
                    (+ x (* delta-e (+ (- y) (- z)))))]
         [dy (λ ([x : Real] [y : Real] [z : Real])
                    (+ y (* delta-e (+ x (* b y)))))]
         [dz (λ ([x : Real] [y : Real] [z : Real])
                   (+ z (* delta-e (+ a (* z (- x c))))))])
    (list (dx x y z)
          (dy x y z)
          (dz x y z))))


;; Lorenz attractor equations
;; '(x y z) -> '(x+dx, y+dy, z+dz)
;; dx = s(y-x)
;; dy = rx-y-xz
;; dz = xy-bz
(: lorenz (-> (Listof Real) (Listof Real)))
(define (lorenz l)
  (let* ([x (first l)]
         [y (second l)]
         [z (third l)]
         [s 10]
         [r 28]
         [b 8/3]
         [dx (λ ([x : Real] [y : Real] [z : Real])
                      (+ x (* delta-e (* s (- y x)))))]
         [dy (λ ([x : Real] [y : Real] [z : Real])
                      (+ y (* delta-e (- (* r x) y (* x z)))))]
         [dz (λ ([x : Real] [y : Real] [z : Real])
                      (+ z (* delta-e (- (* x y) (* b z)))))])
    (list (dx x y z)
          (dy x y z)
          (dz x y z))))


;; Function Point -> List-of-points
;; Creates a list of points based on the given 
;; function and starting point
(: pt-list (-> (-> (Listof Real) (Listof Real))
               (Listof Real)
               (Listof (Listof Real))))
(define (pt-list f start)
  (: iter (-> (Listof (Listof Real))
              Integer
              (Listof (Listof Real))))
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
