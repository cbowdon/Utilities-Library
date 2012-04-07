#lang racket/base
(require racket/math
         racket/generator
         racket/contract)

(provide (contract-out
          ; ->* contract form
          ; groups mandatory args and optional/keyword args with parens
          [file-size-h (->* (path-string?) 
                            (symbol?
                             #:base exact-positive-integer?)
                            inexact-real?)]))

(define (file-size-h pathtofile [unit 'find-best] #:base [base 1000])
  (let ([size-in-bytes (file-size pathtofile)])
    (cond [(> size-in-bytes 0) (unitize size-in-bytes unit base)])))


(define (unitize size-in-bytes unit base)
  (let ([options (hash 
                  'b 0 
                  'kb 1 
                  'mb 2 
                  'gb 3 
                  'tb 4 
                  'pb 5 
                  'find-best (quotient (order-of-magnitude size-in-bytes) 3))])
    (if [hash-has-key? options unit]
        (exact->inexact (/ size-in-bytes (expt base (hash-ref options unit))))
        (error "Size unit not recognised:" unit))))