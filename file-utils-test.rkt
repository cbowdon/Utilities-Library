#lang racket/base
(require rackunit
         "file-utils.rkt")

(test-case
 "file-size-h"
 (check-equal? (file-size-h "testdata.csv" 'b #:base 1024) 143362.0)
 (check-equal? (file-size-h "testdata.csv" 'kb) 143.362)
 (check-equal? (file-size-h "testdata.csv" 'mb) 0.143362)
 (check-equal? (file-size-h "testdata.csv" 'gb) 0.000143362)
 (check-equal? (file-size-h "testdata.csv" 'tb) 1.43362e-07)
 (check-equal? (file-size-h "testdata.csv" 'pb) 1.43362e-10)
 (check-equal? (file-size-h "testdata.csv") 143.362)
 (check-equal? (file-size-h "testdata.csv" #:base 1024) 140.001953125))