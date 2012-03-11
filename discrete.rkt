#lang racket/base
(require racket/contract)

(provide (contract-out
          [discretize (->* ((-> number? number?)) (#:method symbol?) (-> number? number?))]
          [discretize3d (->* ((-> number? number? number?)) (#:method symbol?) (-> number? number? number?))]))

(define (discretize proc #:method [t 'floor])
  (cond [(eq? t 'floor) (λ (x) (proc (floor x)))]
        [(eq? t 'round) (λ (x) (proc (round x)))]
        [(eq? t 'ceiling) (λ (x) (proc (ceiling x)))]
        [else (error "Not a discretization method" t)]))

(define (discretize3d proc3d #:method [t 'floor])
  (cond [(eq? t 'floor) (λ (x y) (proc3d (floor x) (floor y)))]
        [(eq? t 'round) (λ (x y) (proc3d (round x) (round y)))]
        [(eq? t 'ceiling) (λ (x y) (proc3d (ceiling x) (ceiling y)))]
        [else (error "Not a discretization method" t)]))

