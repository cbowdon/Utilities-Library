#lang racket/base
(require racket/list
         racket/stream
         racket/contract
         racket/place
         racket/future
         lib/time)

(provide divide-list
         pmap)

(define (main)
  (define p
    (place 
     ch
     (let-values ([(res ct rt gc) (time-repeat (random 100) #:repeat 1e7)])
       (place-channel-put ch (list res ct rt gc)))))
  (define p1
    (place 
     ch
     (let-values ([(res ct rt gc) (time-repeat (random 100) #:repeat 1e7)])
       (place-channel-put ch (list res ct rt gc)))))
  (values (place-channel-get p) (place-channel-get p1)))

(define/contract (divide-list lst n)
  (-> list? exact-nonnegative-integer? (listof list?))
  (let*-values ([(len) (length lst)]
                [(m r) (quotient/remainder len n)])
    (let loop ([count 0] 
               [remainder r]
               [input lst] 
               [output '()])
      (cond [(= n count) (reverse output)] ;; return divided list
            [(empty? input) (loop (add1 count) 0 '() (cons '() output))] ;; cores exceed list elements: just give empty lists 
            [else 
             (let ([m* (if (> remainder 0) (add1 m) m)])
               (loop (add1 count)
                     (sub1 remainder)
                     (drop input m*)
                     (cons (take input m*) output)))]))))

;; this pmap implementation is only useful if dividing the list is much cheaper than the operation
(define/contract (pmap proc lst [n (processor-count)])
  (->* (procedure? list?) (exact-nonnegative-integer?) list?)
  ;  (define (place-map l)    
  ;    (place ch (place-channel-put ch (map proc l))))
  ;  (for/list ([dl (divide-list lst n)])
  ;    (place-channel-get (place-map dl)))
  '())

;; DOESN'T WORK
;; give up on generalised pmap for now
;; they left it out for a reason