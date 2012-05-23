#lang racket/base
(require racket/list
         racket/string
         racket/vector
         racket/stream
         racket/generator
         racket/contract
         racket/port
         lib/stream)

(provide (contract-out 
          [csv->stream (->* ((or/c string? path?))
                            (#:separators string?)
                            stream?)]
          [csv->list (->* ((or/c string? path?))
                          (#:separators string?)
                          stream?)]
          [file->generator (-> (or/c string? path?) generator?)]
          [string-empty? (-> string? boolean?)]
          [string-not-empty? (-> string? boolean?)]
          [csvs-in-dir (-> (or/c string? path?) (listof path?))]
          [single-column (-> stream? number? stream?)]
          [select-columns (->* (stream? number?)
                               #:rest (listof number?)
                               stream?)]
          [csv-format (-> (or/c stream? vector? (listof (or/c list? vector? stream?))) (or/c vector? stream? (listof string?)))]
          [display-csv (-> (or/c stream? vector? (listof (or/c list? vector? stream?))) port? void?)]
          [write-to-csv-file (-> (or/c stream? vector? (listof (or/c list? vector? stream?)))
                                 (or/c string? path?)
                                 #:exists symbol?
                                 void?)]))

(define (csv->list filename #:separators [sep-expr ",|\t|\r|\n|\r\n|[:cntrl:]"])
  (define (parse-port in)
    (map (λ (x) (parse-by-probable-type (filter string-not-empty? (regexp-split sep-expr x))))
         (port->lines in)))
  (call-with-input-file filename parse-port))

(define (csv->stream filename #:separators [sep-expr ",|\t|\r|\n|\r\n|[:cntrl:]"])
  (stream-map (λ (x) (parse-by-probable-type (filter useful-field? (regexp-split sep-expr x))))
              (stream-filter 
               (λ (x) (not (eof-object? x))) 
               (sequence->stream (in-producer (file->generator filename) 'stop)))))

(define (string-empty? str)
  (equal? 0 (string-length str)))

(define (string-not-empty? str)
  (not (string-empty? str)))

(define (not-nl? x)
  (not (or (equal? x "\n") (equal? x "\r") (equal? x "\r\n"))))

(define (useful-field? x)
  (and (not-nl? x) (string-not-empty? x)))

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

(define (single-column strm index)
  (define ref (if (vector? (stream-first strm)) vector-ref list-ref))
  (stream-map (λ (x) (ref x index)) strm))

(define (csvs-in-dir data-dir)
  (map
   (λ (x) (build-path data-dir x))
   (filter (λ (x) (regexp-match ".csv$" x)) (directory-list data-dir))))

;; file->generator
;; a filename ->
;; a generator that returns successive lines from the file
(define (file->generator filename)
  (generator ()
             ;; cg-helper
             ;; pos ->
             ;; new position and next line
             (define (cg-helper pos)
               (define (line-and-pos port)
                 (begin
                   (file-position port pos)
                   (let ([line (read-line port)])
                     (cons (file-position port) line))))
               (call-with-input-file filename
                 (λ (in) (line-and-pos in))))
             
             ;; fill-buffer
             ;; pos ->
             ;; new position and buffer of lines
             (define (fill-buffer pos)
               (define (lines-and-pos port)
                 (define (l&p-iter count result)
                   (cond [(= 0 count) (cons (file-position port) (reverse result))]
                         [else 
                          (let ([new-line (read-line port)])
                            ;  (if (eof-object? new-line)
                            ;     eof
                            (l&p-iter (sub1 count) (cons new-line result)))]))                 
                 (begin (file-position port pos)
                        (l&p-iter 1000 '())))
               (call-with-input-file filename
                 (λ (in) (lines-and-pos in))))            
             
             ;; gen-iter
             ;; position & buffer ->
             ;; if eof, null
             ;; if buffer empty, new position & new buffer
             ;; else old position and cdr buffer
             (define (gen-iter old)
               (cond [(and (pair? (cdr old)) (eof-object? (cadr old))) 'stop]
                     [(null? (cdr old))
                      (let ([new (fill-buffer (car old))])
                        (begin (yield (cadr new))
                               (gen-iter (cons (car new) (cdr (cdr new))))))]
                     [else 
                      (begin (yield (cadr old))
                             (gen-iter (cons (car old) (cdr (cdr old)))))]))
             
             ;; start the gen-iter
             (gen-iter (cons 0 '()))))


;; utilities for writing to file
(define (csv-format lst)  
  (define (any->string x)
    (cond [(number? x) (number->string x)]
          [(string? x) x]
          [else (error "Not a number or string:" x)]))
  (define (array->string x)
    (string-append 
     (string-join (for/list ([i x]) (any->string i)) ",") 
     (string #\, #\newline)))  
  (for/list ([i lst])
    (array->string i)))

;; UNTESTED
(define (display-csv data out)
  (for ([d (csv-format data)])
    (display d out)))

(define (write-to-csv-file data filename #:exists [exists 'error])
  (call-with-output-file filename #:exists exists
    (λ (out) (display-csv data out))))
;; /UNTESTED

