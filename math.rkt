#lang racket/base

(require racket/contract
         racket/stream)

(provide (contract-out 
          [average (->* () #:rest numeric-data/c number?)]
          [variance (->* () #:rest numeric-data/c number?)]
          [st-dev (->* () #:rest numeric-data/c number?)]
          [sum (->* () #:rest numeric-data/c number?)]
          [poisson-error (->* () #:rest numeric-data/c number?)]
          [gauss (->* (number?)
                      (#:height number? #:mu number? #:sigma number?)
                      number?)]))

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

(define (sum . b)
  (define (s x) (foldl + 0 x))
  (define (ss x) (stream-fold + 0 x))
  (generic s ss b))

;; variance
(define (variance . b)
  (define (var x) (/ (foldl + 0 (map (λ (y) (expt (- y (average x)) 2)) x)) (length x)))
  (define (strvar x) (/ (stream-fold + 0 (stream-map (λ (y) (expt (- y (average x)) 2)) x)) (stream-length x)))
  (generic var strvar b))

;; standard deviation
(define (st-dev . b)
  (sqrt (variance (car b))))

;; poisson-error
(define (poisson-error . b)
  (* (average (car b))
     (/ (sqrt (sum (car b))) (sum (car b)))))

;; gauss
(define (gauss x #:height [h 1] #:mu [u 0] #:sigma [o 1])
  (* h (exp (/ (* -1 (expt (- x u) 2))
               (* 2 o o)))))