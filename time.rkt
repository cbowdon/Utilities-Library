#lang racket/base

(provide time-repeat)

(define-syntax time-repeat
  (syntax-rules ()
    [(time-repeat expr) (time (begin expr #t))]
    [(time-repeat expr #:repeat x)
     (time (begin (for ([i (in-range x)]) expr) #t))]))
