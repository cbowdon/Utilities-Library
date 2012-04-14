#lang racket

(require lib/math
         rackunit)

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
 "poisson-error" 
 (check-equal? (poisson-error 100) 10)
 (check-equal? (poisson-error (build-list 10 (λ (x) 10))) 1))

(test-case
 "gauss height"
 (check-equal? (gauss 0 #:height 1 #:mu 0 #:sigma 1) 1)
 (check-equal? (gauss 1 #:height 10 #:mu 1 #:sigma 100) 10)
 ;; and default args are height = 1, mu = 0, sigma = 1:
 (check-equal? (gauss 0) 1))

(require plot)
(plot (list (function gauss -5 5)
            (function (λ (x) (gauss x #:height 1 #:mu -2 #:sigma 3)) -5 5)))
 

