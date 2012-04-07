#lang racket/base

(require racket/contract
         racket/stream)

(provide (contract-out 
          [average (->  numeric-data/c number?)]
          [variance (-> numeric-data/c number?)]
          [st-dev (-> numeric-data/c number?)]))

(define numeric-data/c (or/c number? (listof number?) stream?))

;; base for parsing numeric-data/c
(define (generic normal-proc stream-proc b)
;  (printf "~a\t~a\t~a\n" normal-proc stream-proc b)
  (cond [(number? (car b)) (normal-proc b)]
        [(list? (car b)) (normal-proc (car b))]
        [(stream? (car b)) (stream-proc (stream-first b))]))
  
;; ronseal
(define (average . b)
  (define (avg x) (/ (foldl + 0 x) (length x)))  
  (define (stravg x) (/ (stream-fold + 0 x) (stream-length x)))
  (generic avg stravg b))
        

;; variance
(define (variance . b)
  (define (var x) (/ (foldl + 0 (map (λ (y) (expt (- y (average x)) 2)) x)) (length x)))
  (define (strvar x) (/ (stream-fold + 0 (stream-map (λ (y) (expt (- y (average x)) 2)) x)) (stream-length x)))
  (generic var strvar b))

;; standard deviation
(define (st-dev . b)
  (sqrt (variance (car b))))

;(require rackunit)
;
;(test-case
; "average"
; (check-equal? (average 1 2 3 4) 5/2)
; (check-equal? (average '(1 2 3 4)) 5/2)
; (check-equal? (average (stream 1 2 3 4)) 5/2))
;
;(test-case
; "wiki"
; (let ([data (list 2 4 4 4 5 5 7 9)])
;   (check-equal? (average data) 5)
;   (check-equal? (variance data) 4)
;   (check-equal? (st-dev data) 2)))