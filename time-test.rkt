#lang racket

(require rackunit
         lib/time)

(define n 1e6)

(define (test n)
  (for ([i (in-range 3)]) (collect-garbage))
  (let ([t0 (current-memory-use)])    
    (for/list ([i (in-range n)]) (random (add1 i))) 
    (- (current-memory-use) t0)))

(test-case
 "memory-usage"
 (let ([long-hand (test n)]
       [macro (memory-usage (for/list ([i (in-range n)]) (random (add1 i))))])
   (check-true (< 1e5 (abs (- long-hand macro))))))
