#lang racket/base
(require racket/list)

(provide time-repeat)

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


(time-repeat (random 1000) #:repeat 1000000)