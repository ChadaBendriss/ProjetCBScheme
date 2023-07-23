#lang racket
(require srfi/1) ; Required for string-split
(require racket/file) ; Required for file->lines

(define (readXYZ fileIn)
 (let ((sL (map (lambda s (string-split (car s)))
 (cdr (file->lines fileIn)))))
 (map (lambda (L)
 (map (lambda (s)
 (if (eqv? (string->number s) #f)
 s
 (string->number s))) L)) sL)))

(define (pick-three-random-points Ps)
  (list (list-ref Ps (random (length Ps)))
        (list-ref Ps (random (length Ps)))
        (list-ref Ps (random (length Ps)))))

(define (plane P1 P2 P3)
  (let* ((a (- (* (- (cadr P2) (cadr P1)) (- (car P3) (car P1)))
               (* (- (car P2) (car P1)) (- (cadr P3) (cadr P1)))))
         (b (- (* (- (car P2) (car P1)) (- (caddr P3) (caddr P1)))
               (* (- (caddr P2) (caddr P1)) (- (car P3) (car P1)))))
         (c (- (* (- (cadr P2) (cadr P1)) (- (caddr P3) (caddr P1)))
               (* (- (caddr P2) (caddr P1)) (- (cadr P3) (cadr P1)))))
         (d (+ (* a (car P1)) (* b (cadr P1)) (* c (caddr P1)))))
    (list a b c d)))

(define (support plane points eps)
  (let ((a (car plane))
        (b (cadr plane))
        (c (caddr plane))
        (d (cadddr plane)))
    (define (point-plane-dist p)
      (abs (+ (* a (car p)) (* b (cadr p)) (* c (caddr p)) (- d))))
    (let loop ((points points) (count 0))
      (if (null? points) (cons count plane)
          (if (<= (point-plane-dist (car points)) eps)
              (loop (cdr points) (+ 1 count))
              (loop (cdr points) count))))))

(define (dominantPlane Ps k eps)
  (let loop ((k k) (best-support 0) (best-plane '()))
    (if (= k 0) best-plane
        (let* ((random-points (pick-three-random-points Ps))
               (plane (apply plane random-points))
               (support-plane (support plane Ps eps)))
          (if (> (car support-plane) best-support)
              (loop (- k 1) (car support-plane) (cdr support-plane))
              (loop (- k 1) best-support best-plane))))))

(define (ransacNumberOfIterations confidence percentage)
  (ceiling (/ (log (- 1 confidence)) (log (- 1 (expt percentage 3))))))

(define (planeRANSAC filename confidence percentage eps)
  (let ((Ps (readXYZ filename)))
    (let ((k (ransacNumberOfIterations confidence percentage)))
      (dominantPlane Ps k eps))))