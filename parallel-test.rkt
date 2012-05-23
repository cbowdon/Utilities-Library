#lang racket

(require rackunit
         lib/parallel)

(test-case
 "divide-lists"
 (let (;; all possible numbers to split
       [t1 (list 1 2 3 4 5)]       
       [a1-1 (list (list 1 2 3 4 5))]
       [a1-2 (list (list 1 2 3) (list 4 5))]
       [a1-3 (list (list 1 2) (list 3 4) (list 5))]
       [a1-4 (list (list 1 2) (list 3) (list 4) (list 5))]
       [a1-5 (list (list 1) (list 2) (list 3) (list 4) (list 5))]
       [a1-6 (list (list 1) (list 2) (list 3) (list 4) (list 5) (list ))]
       ;; longer list
       [t2 (list 1 2 3 4 5 6 7 8 9 10)]       
       [a2-3 (list (list 1 2 3 4) (list 5 6 7) (list 8 9 10))]
       [a2-4 (list (list 1 2 3) (list 4 5 6) (list 7 8) (list 9 10))]
       [a2-5 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8) (list 9 10))])
   (check-equal? (divide-list t1 1) a1-1)
   (check-equal? (divide-list t1 2) a1-2)
   (check-equal? (divide-list t1 3) a1-3)
   (check-equal? (divide-list t1 4) a1-4)
   (check-equal? (divide-list t1 5) a1-5)
   (check-equal? (divide-list t1 6) a1-6)
   (check-equal? (divide-list t2 3) a2-3)
   (check-equal? (divide-list t2 4) a2-4)
   (check-equal? (divide-list t2 5) a2-5)))

(test-case
 "pmap"
 (let* ([proc sqrt]
        [lst (build-list 1000 values)]
        [ans (map proc lst)])
   (check-equal? (pmap proc lst) ans)))
   
   