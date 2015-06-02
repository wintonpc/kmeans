(define-record point (x y))
(define-record centroid (point))
(define-record cpoint (point index centroid))
(define-record cluster (centroid-point points))

(define *points* (list
                  (make-point 1.5 1.1)
                  (make-point 1.6 0.9)
                  (make-point 1.4 1.0)
                  (make-point 20.2 20.5)))

(define (sum xs)
  (fold-left + 0 xs))

(define (mean xs)
  (/ (sum xs) (length xs)))

(define (center-of-mass points)
  (make-point (mean (map point-x points)) (mean (map point-y points))))

(define (k-means n points)
  (define (points->cpoints points)
    (let lp ([points points] [i 0])
      (if (null? points)
          '()
          (cons (make-cpoint (car points) i #f) (lp (cdr points) (+ i 1))))))
  
  (define (init-centroids n points)
    (define (first-centroid points)
      (make-centroid (car points)))
    (define (farthest-from repellers points)
      
    (let lp ([n (- n 1)] [centroids (list (first-centroid points))])
      (if (= n 0)
          centroids
          (lp (- n 1) (cons (make-centroid (farthest-from (map centroid-point centroids) points)) centroids)))))
  
  (let refine ([ps (points->cpoints points)]
               [centroids (init-centroids n points)])
    (let-values ([(reassigned-ps changed?) (assign-points ps centroids)])
      (if changed?
          (refine reassigned-ps (recenter centroids reassigned-ps))
          (make-clusters reassigned-points)))))

(printf "~a~n" (k-means 2 *points*))
