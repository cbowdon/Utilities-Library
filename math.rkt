#lang racket/base

(require racket/contract
         racket/stream
         racket/vector
         racket/list
         lib/stream)

(provide (contract-out
          [!= (-> number? number? boolean?)]
          [sqr (-> number? number?)]
          [average (->* () #:rest numeric-data/c number?)]
          [weighted-average (-> (or/c (listof number?) stream?) (or/c (listof number?) stream?) number?)]
          [variance (->* () #:rest numeric-data/c number?)]
          [st-dev (->* () #:rest numeric-data/c number?)]
          [sum (->* () #:rest numeric-data/c number?)]
          [propagate-errors (->* (number?) #:rest numeric-data/c number?)]
          [poisson-error (->* () #:rest numeric-data/c number?)]
          [gauss (->* (number?)
                      (#:height number? #:mu number? #:sigma number?)
                      number?)]
          [linear-regression (-> (listof vector?) vector?)]
          [make-linear-equation (->* () #:rest (or/c vector? list?) procedure?)]
          [column-sort (-> procedure? (listof vector?) exact-nonnegative-integer? (listof vector?))]
          [rank (->* 
                 ((listof (or/c number? (listof number?) vector?)))
                 (#:column exact-nonnegative-integer?)
                 (listof number?))]
          [pearson (-> (listof vector?) number?)]
          [spearman (-> (listof vector?) number?)]))

(define numeric-data/c (or/c number? (listof number?) stream?))

(define (!= x y) (not (= x y)))
(define (sqr x) (* x x))

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

(define (weighted-average weights items)  
  (/ (sum (stream-mapx * weights items))
     (sum weights)))

;; variance
;; var is horrendous for memory consumption
;; fvar better - TODO: re-write with for/foldl
;; strvar not so bad
(define (variance . b)
  (define (var x) (/ (foldl + 0 (map (λ (y) (expt (- y (average x)) 2)) x)) (length x)))
  (define (fvar x)    
    (/ (for/fold ([sum 0]) ([i x]) (+ sum (sqr (- i (average x))))) 
       (length x)))
  (define (strvar x) (/ (stream-fold + 0 (stream-map (λ (y) (expt (- y (average x)) 2)) x)) (stream-length x)))
  (generic fvar strvar b))

;; standard deviation
(define (st-dev . b)
  (sqrt (variance (car b))))

;; error propagation (a.k.a. "summing in quadrature")
(define (propagate-errors a . b)
  (sqrt (sum (map sqr (cons a b)))))

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
  (cond [(vector? (car e)) (λ (x) (+ (* x (vector-ref (car e) 0)) (vector-ref (car e) 1)))]
        [(and (list? e) (number? (car e))) (λ (x) (+ (* x (car e)) (cadr e)))]
        [(and (list? e) (list? (car e))) (λ (x) (+ (* x (caar e)) (cadar e)))]
        [else (error "don't understand input" e)]))



;; competitive ranking, with proper averages for ties
(define (rank sorted-list #:column [col 0])
  ;; only traverses the list once, i.e. O(n)
  ;; uses two mutually recursive functions
  ;; the first is a simple counter, until a tie (repeat) is spotted
  ;; the second works out the average ranking for the repeats before giving back
  
  ;; accessors
  (define (accessor-type lst-acc index)
    (cond [(or (empty? sorted-list) (number? (car sorted-list))) lst-acc]          
          [(vector? (car sorted-list)) (λ (x) (vector-ref (lst-acc x) index))]          
          [(list? (car sorted-list)) (λ (x) (list-ref (lst-acc x) index))]
          [else (error "Unknown input type:" (first sorted-list))]))
  (define get1st (accessor-type first col))
  (define get2nd (accessor-type second col))
  
  ;; if first two elements not equal, should return cons index+1 onto result
  ;; else should divert to when-same
  ;; rank of first item, input seq, results to cons onto
  ;; ->
  ;; rank+1, rest seq, cons'd results
  ;; possible states: input can be empty, no repeats or repeats
  (define (rank-iter n input result)
    ;(printf "~a~n" (first input))
    (cond [(empty? input) (reverse result)]
          [(empty? (cdr input)) (reverse (cons n result))]
          ;; repeat found: pass directly to when-same
          [(= (get1st input) (get2nd input)) (when-same n input result (get1st input))]
          ;; no repeat: add rank=n to results, inc n, move down list
          [else (rank-iter (add1 n) (rest input) (cons n result))]))
  
  ;; should count how many similar results are in given list and return cons-repeat of their average rank
  ;; rank of first item, repeating term, input seq (inc all repeats), results to cons onto
  ;; ->
  ;; seq not inc repeats, rank of first item in new seq, consed results
  ;; possible states: seq can be too short, no repeats, one repeat [or multiple repeats]
  (define (when-same n-of-first input-list result repeat-term)  
    (define (new-result n-same sum-same)    
      (cons-repeat (/ sum-same n-same) result #:repeat n-same))
    (define (ws-iter n n-same sum-same input)
      (cond [(or (empty? input) (!= (get1st input) repeat-term))
             ;; empty/no repeat: pass to rank-iter with appropriate n, input and result w/ avgs
             (rank-iter (add1 n) input (new-result n-same sum-same))]
            [else 
             ;; repeat: add to tally, inc n, move down list
             (ws-iter (add1 n) (add1 n-same) (+ sum-same n 1) (rest input))]))
    (ws-iter n-of-first 1 n-of-first (rest input-list)))
  
  ;; utility: faster than appending I hope
  ;; TODO: profile vs (append (make-list n x) results)
  (define (cons-repeat a b #:repeat [n 1])
    (let loop ([count n]
               [result b])
      (if [< 0 count]
          (loop [sub1 count]
                [cons a result])
          result)))
  
  ;; start
  (rank-iter 1 sorted-list '()))

(define (pearson list-of-vectors)  
  (define (single-col lv index) (map (λ (x) (vector-ref x index)) lv))
  (define (ps as bs a0 b0) (foldl + 0 (map (λ (a b) (* (- a a0) (- b b0))) as bs)))
  (define (ss as a0) (foldl + 0 (map (λ (a) (* (- a a0) (- a a0))) as)))
  (let* ([xs (single-col list-of-vectors 0)]
         [ys (single-col list-of-vectors 1)]
         [x0 (average xs)]
         [y0 (average ys)]
         [dnm1 (ss xs x0)]
         [dnm2 (ss ys y0)])
    (if [or (= 0 dnm1) (= 0 dnm2)] ; zero correlation
        0
        (/ (ps xs ys x0 y0) 
           (* (sqrt dnm1) (sqrt dnm2))))))

(define (column-sort proc list-of-vectors column-index)
  (sort list-of-vectors 
        (λ (v1 v2) (proc (vector-ref v1 column-index) (vector-ref v2 column-index)))))

(define/contract (rank-each-column proc data)
  (-> (listof vector) procedure? (listof vector))
  ; for every column, get sort-and-rank
  (define vlen (vector-length (first data)))
  (for/list ([i (in-range vlen)])
    (unsorted-rank proc data i))
  '())

(define (unsorted-rank proc data c)
  (rank (column-sort proc data c) #:column c)) 

(define/contract (index lst)
  (-> (listof vector) (listof vector))
  (define vlen (add1 (vector-length (first lst)))) 
  (let loop ([count 0]
             [input lst])
    (cond [(empty? input) '()]
          [else            
           (cons (build-vector 
                  vlen 
                  (λ (x) (if (= 0 x) count (vector-ref (first input) (sub1 x)))))
                 (loop (add1 count)
                       (rest input)))])))

(define k (build-list 10 (λ (x) (vector (random 20) (random 20)))))

(define (spearman list-of-vectors)
  (define (x-< v1 v2)
    (< (vector-ref v1 0) (vector-ref v2 0)))
  (let ([x-sorted (sort list-of-vectors x-<)])
    ;; assign ranks
    ;; do spearman sum: i.e. do pearson's on the ranks
    1.0
    ))

;(for ([i (in-range 3)]) (collect-garbage))