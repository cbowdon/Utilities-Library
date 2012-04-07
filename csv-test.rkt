#lang racket
(require rackunit   
         profile
         plot
         "csv.rkt")

;(define-test-suite csv-tests
  
  (test-case
   "csv->stream all lines correct"
   (check-equal? (stream-length (csv->stream "testdata.csv")) 3248)
   (check-equal? (stream-first (csv->stream "testdata.csv")) (list 337316 27.7 34000 73377 9594870 5024327)))
  
  (test-case
   "select-columns works"
   (let ([data (csv->stream "testdata.csv")])
     (check-true (stream? (select-columns data 1)))
     (check-equal? (stream-ref (select-columns data 1) 0) 27.7)
     (check-equal? (stream-ref (select-columns data 1 2) 0) (vector 27.7 34000))))
  ;)

;(run-test csv-tests)
;
;; note to self
;; here's how to do basic 3d plotting
;; the 2d version is same process but without the "3d" suffixes
;(define (plot-thunk)
;  (begin
;    
;    ; parameterize is a sort of let form
;    (parameterize ([plot-title "Test plot"]
;                   [plot-x-label "Time (ms)"]
;                   [plot-y-label "Temp (C)"]
;                   [plot-z-label "Counts"])
;      
;      ; get data 
;      (define data (csv->stream "testdata.csv"))
;      ; make some lists of 3D vectors
;      (define a3d (stream-map (lambda (x) (vector (first x) (second x) (fourth x))) data))
;      (define b3d (stream-map (lambda (x) (vector (first x) (second x) (fifth x))) data))
;      ; pass these to plot, via points
;      (plot3d (list                
;               ;(points3d (stream->list a3d) #:sym "α" #:color 'red #:label "alpha")
;               (points3d (stream->list b3d) #:sym "β" #:color 'green #:label "beta")) 
;              #:angle 0
;              #:altitude 0))))
;
;;(profile-thunk plot-thunk)
;(plot-thunk)