#lang racket/base
(require racket/stream
         racket/contract)

(provide stream-mapx
         streamof
         (contract-out
          [stream-print (-> (or/c stream? list?) any)]))

(define-syntax stream-mapx
  (syntax-rules ()
    [(stream-mapx proc a) (stream-map proc a)]
    [(stream-mapx proc a b ...) (stream-iter proc (list a b ...) empty-stream)]))

(define (stream-iter proc los result)
  (cond [(ormap stream-empty? los) result]
        [else
         (stream-iter
          proc
          (map stream-rest los)
          (stream-append 
           result
           (stream-cons (apply proc (map stream-first los)) empty-stream)))]))

(define (stream-print x)
  (stream-for-each displayln x))

; returns a predicate that is true where the argument is a stream of type c
(define (streamof c?) (lambda (x) (and (stream? x) (stream-andmap c? x))))
