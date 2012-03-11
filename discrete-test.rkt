#lang racket/base

(require rackunit
         racket/stream
         Lib/discrete)

(test-case
 "Discretize"
 (let ([dsin (discretize sin)])
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin -1.0))) (in-range -1.0 0.0 0.1))
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin 0.0))) (in-range 0.0 1.0 0.1))
   (stream-for-each (λ (x) (check-equal? (dsin x) (sin 1.0))) (in-range 1.0 2.0 0.1))))

(require plot)

(plot (list (tick-grid)
            (function sin -6 6)
            (function (discretize sin) -6 6)))