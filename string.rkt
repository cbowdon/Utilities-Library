#lang racket/base

(require racket/contract)

(provide (contract-out [pad-string (->* (string? exact-nonnegative-integer?) (char?) string?)]))

(define (pad-string str int [pad-char #\ ])  
  (let ([strlen (string-length str)])
    (if (< strlen int)
        (string-append (build-string (- int strlen) (λ (x) pad-char)) str)
        str)))