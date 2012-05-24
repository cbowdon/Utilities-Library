#lang racket
(require rackunit   
         profile
         lib/csv
         lib/time)

(define test-data "testdatalong.csv")

;;; Discovered this module on PLaneT:
;(require (planet neil/csv))
;
;(define (neil-csv) (begin (csv->list (open-input-file test-data)) #t))
(define (lib-csv) (begin (stream->list (csv->stream test-data)) #t))
(define (lib-csv2) (begin (csv->listx test-data) #t))
;;
;; where test-data is a 30MB, 114,000-line CSV file
;;; background mem ~25,000
;(printf "neil-csv\t~a~n" (time (neil-csv))) ;; mem 348,000
(printf "lib-csv\t~a~n" (time (lib-csv))) ;; mem 203,000
(printf "lib-csv2\t~a~n" (time (lib-csv2))) ;; mem 290,000 

;; Profiling results (Solid State Drive):
;;
;; CPU speed ratios (using time, est. 5% error)
;; neil/csv->list 1.00
;; Lib/csv->list 1.03
;; Lib/csv->stream 1.74
;;
;; Memory usage ratios (using Windows 7's Resource Monitor, est. 5% error)
;; neil/csv->list 1.00
;; Lib/csv->list 0.82
;; Lib/csv->stream 0.55
;;
;; Lib/csv->stream on SSD:
;; Time to fill-buffer is O(n) for n accesses
;; Time to fill-one-buffer is just over O(n) for n lines in buffer


(test-case
 "csv->stream all lines correct"
 (check-equal? (stream-length (csv->stream "testdata.csv")) 3248)
 (check-equal? (stream-first (csv->stream "testdata.csv")) (list 337316 27.7 34000 73377 9594870 5024327)))

(test-case
 "select-columns"
 (let ([data (csv->stream "testdata.csv" #:separators ",")])
   (check-true (stream? (select-columns data 1)))
   (check-equal? (stream-ref (select-columns data 1) 0) 27.7)
   (check-equal? (stream-ref (select-columns data 1 2) 0) (vector 27.7 34000))))

(test-case 
 "single-column"
 (let ([data (select-columns (csv->stream "testdata.csv") 1 2 3)])
   (check-true (stream? (single-column data 2)))
   (check-true (number? (stream-first (single-column data 2))))
   (check-equal? (stream-first (single-column data 2)) 73377)))

;; fill-buffer
;; pos ->
;; new position and buffer of lines
(define (fill-buffer pos n)
  (define (lines-and-pos port)
    (define (l&p-iter count result)
      (cond [(= 0 count) (cons (file-position port) (reverse result))]
            [else 
             (let ([new-line (read-line port)])
               ;  (if (eof-object? new-line)
               ;     eof
               (l&p-iter (sub1 count) (cons new-line result)))]))                 
    (begin (file-position port pos)
           (l&p-iter n '())))
  (call-with-input-file test-data
    (λ (in) (lines-and-pos in))))            

(define (test nreps)
  (let ([n nreps])
    (for ([i (in-range 1 n)])    
      (printf "~a\t~a\t" (expt 10 i) (expt 10 (- n i 1)))
      (time-repeat (fill-buffer 10000000 (expt 10 i)) #:repeat (expt 10 (- n i 1))))))

(test-case
 "csv-format"
 (let ([list-input (list 
                    (build-list 10 values)
                    (list 10 11 12 13 14 15 16 "-" "-" 19))]
       [vector-input (list 
                      (build-vector 10 values)
                      (vector 10 11 12 13 14 15 16 "-" "-" 19))]
       [output (list
                "0,1,2,3,4,5,6,7,8,9,\n"
                "10,11,12,13,14,15,16,-,-,19,\n")])
   (check-equal? (csv-format list-input) output)
   (check-equal? (csv-format vector-input) output)))