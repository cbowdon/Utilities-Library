#lang racket/base

(require rackunit
         racket/stream
         Lib/discrete)

;; enumeration function
; surely I'm reinventing the wheel here?
(define (enumerate min max inc)
  (define (en-iter current ls)
    (if [<= current max]
        (en-iter (+ current inc) (cons current ls))
        ls))
  (reverse (en-iter min '())))

;; test cases

(test-case
 "Discretize 2d"
 (let ([dsin (discretize sin)])
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin -1.0))) (in-range -1.0 0.0 0.1))
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin 0.0))) (in-range 0.0 1.0 0.1))
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin 1.0))) (in-range 1.0 2.0 0.1))))

(test-case
 "Discretize 3d"
 (let* ([surf (λ (x y) (+ (cos x) (sin y)))]
        [dsurf (discretize3d surf)])
   (for-each (λ (x y) (check-equal? (dsurf x y) (surf -1.0 -1.0))) (enumerate -1.0 0.0 0.1) (enumerate -1.0 0.0 0.1))
   (for-each (λ (x y) (check-equal? (dsurf x y) (surf 0.0 0.0))) (enumerate 0.0 1.0 0.1) (enumerate 0.0 1.0 0.1))
   (for-each (λ (x y) (check-equal? (dsurf x y) (surf 1.0 1.0))) (enumerate 1.0 2.0 0.1) (enumerate 1.0 2.0 0.1))))


;; visualization

(require plot)

(plot (list (tick-grid)
            (function sin -6 6)
            (function (discretize sin #:method 'round) -6 6)))

(plot3d (list (surface3d (λ (x y) (+ (sin x) (cos y))) -6 6 -6 6 #:color 'red #:alpha 0.5)
              (surface3d (discretize3d (λ (x y) (+ (sin x) (cos y))) #:method 'round) -6 6 -6 6 #:color 'green #:alpha 0.5)))