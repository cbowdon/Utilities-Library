#lang racket

(require lib/math
         rackunit)

(define (check-vector-equal? v1 v2 eps)
  (define (eq-w/err? a b) (= (vector-length v1) 
                             (vector-length v2) 
                             (vector-count (λ (x y) (> eps (abs (- x y)))) a b)))
  (check eq-w/err? v1 v2 (format "v1:~a not equal to v2:~a" v1 v2)))

(test-case
 "average"
 (check-equal? (average 1 2 3 4) 5/2)
 (check-equal? (average '(1 2 3 4)) 5/2)
 (check-equal? (average (stream 1 2 3 4)) 5/2))

(test-case
 "sum, average, variance and stdev (source = wiki)"
 (let ([data (list 2 4 4 4 5 5 7 9)])
   (check-equal? (sum data) (foldl + 0 data))
   (check-equal? (average data) 5)
   (check-equal? (variance data) 4)
   (check-equal? (st-dev data) 2)))

(test-case
 "propagate-errors"
 (check-equal? (propagate-errors 0.1 0.5 0.3) (sqrt (+ 0.01 0.25 0.09))))

(test-case
 "weighted-average"
 (let ([zeff 5.232447701411555]
       [meff 161.95259051810365]
       [weights '(0.4 13.71 41.96 33.43 8.75 1.38 0.07 0.01 0.27)]
       [carbons (stream->list (in-range 9 18))])
   (define (hcMass n)
     (+ (* 12 n) (* 2 n) 2))
   (define (hcZeff n)
     (/ (+ (* 12 n 6) (* 2 n) 2) 
        (hcMass n)))
   (check-equal? (weighted-average weights (map hcMass carbons)) meff)
   (check-equal? (weighted-average weights (map hcZeff carbons)) zeff)))


(test-case
 "poisson-error" 
 (check-equal? (poisson-error 100) 10)
 (check-equal? (poisson-error (build-list 10 (λ (x) 10))) 1))

(test-case
 "gauss height"
 (check-equal? (gauss 0 #:height 1 #:mu 0 #:sigma 1) 1)
 (check-equal? (gauss 1 #:height 10 #:mu 1 #:sigma 100) 10)
 ;; and default args are height = 1, mu = 0, sigma = 1:
 (check-equal? (gauss 0) 1))

(test-case
 "linear regression"
 (let ([datasetA '( #(1 0.5) #(2 1.0) #(3 1.5) #(4 2.0))]
       [datasetB '( #(1 1.0) #(2 2.0) #(3 3.0) #(4 4.0))]
       [datasetC '( #(0 0.5) #(1 1.0) #(2 1.5) #(3 2.0))])
   (check-vector-equal? (linear-regression datasetA) #(0.5 0.0) 1e-4)
   (check-vector-equal? (linear-regression datasetB) #(1.0 0.0) 1e-4)
   (check-vector-equal? (linear-regression datasetC) #(0.5 0.5) 1e-4)))

(test-case
 "make-linear-equation"
 (check-equal? ((make-linear-equation 0.5 2) 1) 2.5)
 (check-equal? ((make-linear-equation #(0.5 2)) 1) 2.5)
 (check-equal? ((make-linear-equation '(0.5 2)) 1) 2.5))

(test-case
 "rank"
 (let ([sorted-lists
        (list '(0 1 2 3 4)
              '(0 1 1 2 5)
              '(0 0 0 0 1)
              '(1 2 2 2 3)
              '(1 2 2 3 3)
              '(1 2 3 4 5 6 7)
              '(1 1 2 2 5 6 7)
              '(1 1 1 3 5 6 7)
              '(1 1 1 1 5 6 6)
              '(1 1 1 1 1 1 1)
              '(1)
              '())]
       [answers
        (list '(1 2 3 4 5)
              '(1 5/2 5/2 4 5)
              '(5/2 5/2 5/2 5/2 5)
              '(1 3 3 3 5)
              '(1 5/2 5/2 9/2 9/2)
              '(1 2 3 4 5 6 7)
              '(3/2 3/2 7/2 7/2 5 6 7)
              '(2 2 2 4 5 6 7)
              '(5/2 5/2 5/2 5/2 5 13/2 13/2)
              '(4 4 4 4 4 4 4)
              '(1)
              '())])
   (for-each
    (λ (x y) (check-equal? (rank x) y))
    sorted-lists
    answers)))

(define test-array 
  (map vector
       (build-list 10 (λ (x) (random 100)))
       (build-list 10 (λ (x) (random 100)))))



(test-case
 "pearson"
 (let* ([x (build-list 10 values)]
        [y (build-list 10 (λ (i) (* i 2)))]
        [z (build-list 10 (λ (i) (random 10)))]
        [a (make-list 10 3)]
        [data0 (map vector x y)]
        [data1 (map vector x z)]
        [data2 (map vector x a)])
   (check-equal? (pearson data0) 1.0)
   (check-true (< (abs (pearson data1)) 0.8))
   (check-equal? (pearson data2) 0)))



;(require plot)
;(test-case
; "spearman's rank"
; (let* ([n 10]
;        [correlated-data (map (λ (x) (vector x (sin x))) (build-list n values))]
;        [semi-correlated-data (map (λ (x) (vector x (random (add1 x)))) (build-list n values))]
;        [uncorrelated-data (map (λ (x) (vector x 2.5)) (build-list n values))])
;   (check-equal? (spearman correlated-data) 1.0)
;   (check-equal? (spearman semi-correlated-data) 0.5)
;   (check-equal? (spearman uncorrelated-data) 0.0)
;   (plot (list
;          (points correlated-data #:color 'green)
;          (points semi-correlated-data #:color 'orange)
;          (points uncorrelated-data #:color 'red)))
;   ))


