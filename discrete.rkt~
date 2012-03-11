#lang racket/base
(require racket/contract)

(provide (contract-out
          [discretize (-> (-> number? number?) (-> number? number?))]))

; discretize 
; TODO
; generalize this to rest arguments
(define (discretize proc)
  (λ (x) (proc (floor x))))

