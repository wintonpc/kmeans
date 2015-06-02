;;#lang racket

;;(provide (all-defined-out))
(require rackunit)

(struct point (x y) #:transparent)
(struct centroid (point) #:transparent)
(struct cpoint (point index centroid) #:transparent)
(struct cluster (centroid-point points) #:transparent)

(define *points* (list
                  (point 1.5 1.1)
                  (point 1.6 0.9)
                  (point 1.4 1.0)
                  (point 20.2 20.5)))

(define (sum xs)
  (foldl + 0 xs))

(define (mean xs)
  (/ (sum xs) (length xs)))

(define (center-of-mass points)
  (point (mean (map point-x points)) (mean (map point-y points))))

(define (points->cpoints points)
  (let lp ([points points] [i 0])
    (if (null? points)
        '()
        (cons (cpoint (car points) i #f) (lp (cdr points) (+ i 1))))))

(define (init-centroids n points)
  (let lp ([n (- n 1)] [centroids (list (first-centroid points))])
    (if (= n 0)
        centroids
        (let ([farthest-centroid (centroid (farthest-from (map centroid-point centroids) points))])
          (lp (- n 1) (cons  centroids))))))

(define (first-centroid points)
  (centroid (car points)))

(define (closest-repeller repellers point)
  (let ([dists (map (lambda (r) (cons r (dist r point))))])
    (minp cdr dists)))

(define (minp p xs)
  (let lp ([xs xs]
           [min #f])
    (;

(define (square x)
  (* x x))

(define (dist a b)
  (let ([dx (- (point-x a) (point-x b))]
        [dy (- (point-y a) (point-y b))])
    (+ (square dx) (square dy))))

;;(define (farthest-from repellers points)
;;  (let ([pd (map closest-repeller points)])
    

(define (k-means n points)
  (let refine ([ps (points->cpoints points)]
               [centroids (init-centroids n points)])
    (let-values ([(reassigned-ps changed?) (assign-points ps centroids)])
      (if changed?
          (refine reassigned-ps (recenter centroids reassigned-ps))
          (make-clusters reassigned-points)))))



;; TESTS

;; sum
(check-equal? (sum '(3 0.14 0.0015)) 3.1415)

;; mean
(check-equal? (mean '(1 2 3 6)) 3)

;; center-of-mass
(let ([points (list (point 1.5 1.1)
                    (point 1.6 0.9)
                    (point 1.4 1.0))])
  (check-equal? (center-of-mass points)
                (point (mean '(1.5 1.6 1.4)) (mean '(1.1 0.9 1.0)))))

;; points->cpoints
(check-equal? (points->cpoints (list (point 3 4) (point 5 6)))
              (list (cpoint (point 3 4) 0 #f) (cpoint (point 5 6) 1 #f)))

;; first-centroid
(check-equal? (first-centroid (list (point 3 4) (point 5 6)))
              (centroid (point 3 4)))

;; dist
(check-equal? (dist (point 1 2) (point 3 -4)) 40)

;; closest-repeller
(let ([a (point 0 1)]
      [b (point 1 2)]
      [c (point 5 5)])
  (check-equal? (closest-repeller (list a b) c)
                (cons b (dist b c))))
