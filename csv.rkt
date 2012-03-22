#lang racket/base
(require racket/stream
         racket/port
         racket/contract
         "stream-utils.rkt")

(provide (contract-out 
          [csv->stream (-> (or/c string? path?) stream?)]
          [split-string (-> string? (or/c string? char?) (listof string?))]
          [string-empty? (-> string? boolean?)]
          [string-not-empty? (-> string? boolean?)]
          [csvs-in-dir (-> (or/c string? path?) (listof path?))])
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

(define (select-columns strm a . b)
  (cond [(null? b) (stream-map (λ (x) (list-ref x a)) strm)]
        [else (stream-map 
               (λ (x) (list->vector (map (λ (i) (list-ref x i)) (cons a b)))) 
               strm)]))


(define (csvs-in-dir data-dir)
  (map
   (λ (x) (build-path data-dir x))
   (filter (λ (x) (regexp-match ".csv$" x)) (directory-list data-dir))))

;(require plot)
;(define-syntax plot-columns
 ; (syntax-rules ()
  ;  [(plot-columns strm a b) (plot (points (select-columns strm a b)))]
   ; [(plot-columns strm a b c) (plot3d (points3d (select-columns strm a b c)))]))

;(define k (csv->stream "testdata.csv"))