#lang racket/base

(provide time-repeat
         memory-usage)

;(define-syntax time-repeat
;  (syntax-rules ()
;    [(time-repeat expr) (time (begin expr #t))]
;    [(time-repeat expr #:repeat x)
;     (time (begin (for ([i (in-range x)]) expr) #t))]))

(define-syntax time-repeat
  (syntax-rules ()
    [(time-repeat expr) 
     (begin
       (for ([i (in-range 3)]) (collect-garbage))       
       (time-apply (λ (x) expr) (list #t)))]
    [(time-repeat expr #:repeat n)
     (begin
       (for ([i (in-range 3)]) (collect-garbage))       
       (for/fold ([last-result '()] [cpu-time 0] [real-time 0] [gc-time 0]) ([i (in-range n)])       
         (let-values ([(res ct rt gt) (time-apply (λ (x) expr) (list #t))])
           (values res
                   (+ cpu-time ct)
                   (+ real-time rt)
                   (+ gc-time gt)))))]))

(define-syntax memory-usage 
  (syntax-rules ()
    [(memory-usage expr)
     (begin
       (for ([i (in-range 3)]) (collect-garbage))
       (let ([m0 (current-memory-use)])
         expr 
         (- (current-memory-use) m0)))]))
