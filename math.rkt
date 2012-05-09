#lang racket/base

(require racket/contract
         racket/stream
         racket/vector
         racket/list)

(provide (contract-out
          [!= (-> number? number? boolean?)]
          [average (->* () #:rest numeric-data/c number?)]
          [variance (->* () #:rest numeric-data/c number?)]
          [st-dev (->* () #:rest numeric-data/c number?)]
          [sum (->* () #:rest numeric-data/c number?)]
          [poisson-error (->* () #:rest numeric-data/c number?)]
          [gauss (->* (number?)
                      (#:height number? #:mu number? #:sigma number?)
                      number?)]
          [linear-regression (-> (listof vector?) vector?)]
          [make-linear-equation (->* () #:rest (or/c vector? list?) procedure?)]
          [rank (-> (listof number?) (listof number?))]
          [spearman (-> (listof vector?) number?)]))

(define numeric-data/c (or/c number? (listof number?) stream?))

(define (!= x y) (not (= x y)))

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

;; linear-regression (simple)
;; see: http://www.classes.cs.uchicago.edu/archive/2010/fall/10500-1/labs/Lab5/Lab5.html
(define (linear-regression dataset)
  ;; dataset is a list of vectors #(a b)
  (define (sum-vref lst ref) (foldl + 0 (map (λ (x) (vector-ref x ref)) lst)))
  (define (sum-proc lst proc) (foldl + 0 (map (λ (x) (proc x)) lst)))
  ;; variables are common to both 'functions'
  (let ([Ex (sum-vref dataset 0)]
        [Ey (sum-vref dataset 1)]
        [Exy (sum-proc dataset (λ (x) (* (vector-ref x 0) (vector-ref x 1))))]
        [Ex2 (sum-proc dataset (λ (x) (* (vector-ref x 0) (vector-ref x 0))))]
        [n (length dataset)])    
    ;; function for gradient
    (define gradient 
      (/ (- (* n Exy) (* Ex Ey))
         (- (* n Ex2) (* Ex Ex))))        
    ;; function for intercept
    (define intercept 
      (/ (- (* Ey Ex2) (* Ex Exy))
         (- (* n Ex2) (* Ex Ex))))    
    (vector gradient intercept)))

(define (make-linear-equation  . e)  
  (cond [(vector? (car e)) (lambda (x) (+ (* x (vector-ref (car e) 0)) (vector-ref (car e) 1)))]
        [(and (list? e) (number? (car e))) (lambda (x) (+ (* x (car e)) (cadr e)))]
        [(and (list? e) (list? (car e))) (lambda (x) (+ (* x (caar e)) (cadar e)))]
        [else (error "don't understand input" e)]))



;; competitive ranking, with proper averages for ties
(define (rank sorted-list)
  ;; only traverses the list once, i.e. O(n)
  ;; uses two mutually recursive functions
  ;; the first is a simple counter, until a tie (repeat) is spotted
  ;; the second works out the average ranking for the repeats before giving back
  
  ;; if first two elements not equal, should return cons index+1 onto result
  ;; else should divert to when-same
  ;; rank of first item, input seq, results to cons onto
  ;; ->
  ;; rank+1, rest seq, cons'd results
  ;; possible states: input can be empty, no repeats or repeats
  (define (rank-iter n input result)
    (cond [(empty? input) (reverse result)]
          [(empty? (cdr input)) (reverse (cons n result))]
          ;; repeat found: pass directly to when-same
          [(= (car input) (cadr input)) (when-same n input result (car input))]
          ;; no repeat: add rank=n to results, inc n, move down list
          [else (rank-iter (add1 n) (cdr input) (cons n result))]))
  
  ;; should count how many similar results are in given list and return cons-repeat of their average rank
  ;; rank of first item, repeating term, input seq (inc all repeats), results to cons onto
  ;; ->
  ;; seq not inc repeats, rank of first item in new seq, consed results
  ;; possible states: seq can be too short, no repeats, one repeat [or multiple repeats]
  (define (when-same n-of-first input-list result repeat-term)  
    (define (new-result n-same sum-same)    
      (cons-repeat (/ sum-same n-same) result #:repeat n-same))
    (define (ws-iter n n-same sum-same input)
      (cond [(or (empty? input) (!= (car input) repeat-term))
             ;; empty/no repeat: pass to rank-iter with appropriate n, input and result w/ avgs
             (rank-iter (add1 n) input (new-result n-same sum-same))]
            [else 
             ;; repeat: add to tally, inc n, move down list
             (ws-iter (add1 n) (add1 n-same) (+ sum-same n 1) (cdr input))]))
    (ws-iter n-of-first 1 n-of-first (cdr input-list)))
  
  ;; utility: faster than appending I hope
  (define (cons-repeat a b #:repeat [n 1])
    (let loop ([count n]
               [result b])
      (if [< 0 count]
          (loop [sub1 count]
                [cons a result])
          result)))
  
  ;; start
  (rank-iter 1 sorted-list '()))

(define (spearman lv)
  (define (x-< v1 v2)
    (< (vector-ref v1 0) (vector-ref v2 0)))
  (let ([x-sorted (sort lv x-<)])
    ;; sort list by x
    ;; assign rank
    ;; do spearman sum
    1.0
    ))