#lang racket/base
(require racket/stream
         racket/contract)

(provide streamof
         (contract-out
          [stream-mapx (->* (procedure? stream?)                            
                            #:rest stream?
                            stream?)]
          [stream-print (-> (or/c stream? list?) any)]))

;; stream-map for multiple sequences
(define (stream-mapx proc a . b)
  (define (sm-rec los)
    (cond [(ormap stream-empty? los)
           empty-stream]
          [else   
           (stream-cons
            (apply proc (map stream-first los))
            (sm-rec (map stream-rest los)))]))
  (sm-rec (cons a b)))

(define (stream-print x)
  (stream-for-each displayln x))

; returns a predicate that is true where the argument is a stream of type c
(define (streamof c?) (lambda (x) (and (stream? x) (stream-andmap c? x))))