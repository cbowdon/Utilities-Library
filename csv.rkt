#lang racket/base
(require racket/stream
         racket/port
         racket/contract
         "stream-utils.rkt")

(provide (contract-out 
          [csv->stream (-> string? stream?)]
          [split-string (-> string? (or/c string? char?) (listof string?))]
          [string-empty? (-> string? boolean?)]
          [string-not-empty? (-> string? boolean?)])
         select-columns)

(define (csv->stream filename)
  (define (parse-port in)
    (stream-map (lambda (x) (parse-by-probable-type (filter string-not-empty? (split-string x ","))))
                (port->lines in)))
  (call-with-input-file filename parse-port))

(define (split-string str delimiter)
  (regexp-split delimiter str))

(define (string-empty? str)
  (equal? 0 (string-length str)))

(define (string-not-empty? str)
  (not (string-empty? str)))

; listof string? -> listof int/double/str
(define (parse-by-probable-type strlis)
  (define (try-string->number str)
    (let ([num (string->number str)])
      (cond [(number? num) num]
            [else str])))
  (map try-string->number strlis))

; given a csv stream, extract some columns 
; (variable number of indices given) 
; as list of vectors for plotting

(define-syntax select-columns
  (syntax-rules ()
    [(select-columns strm a) (select-single-column strm a)]
    [(select-columns strm a b ...) (select-multiple-columns strm (list a b ...))]))

(define (select-single-column strm index)  
  (stream-map 
   (lambda (x) (vector (list-ref x index))) strm))

(define (select-multiple-columns strm list-of-indices)
  (stream-map 
   (lambda (x) (list->vector                 
                (map 
                 (lambda (i) (list-ref x i)) 
                 list-of-indices))) 
   strm))

(require plot)
(define-syntax plot-columns
  (syntax-rules ()
    [(plot-columns strm a b) (plot (points (select-columns strm a b)))]
    [(plot-columns strm a b c) (plot3d (points3d (select-columns strm a b c)))]))

;(define k (csv->stream "testdata.csv"))