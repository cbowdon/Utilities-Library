#lang racket/base
(require rackunit
         racket/stream
         lib/stream-utils)

(define distance (in-range 0 35 5))
(define shielded '(20 7 3.5 2 1.5 1 1))
(define unshielded '(75 25 13 7.5 4 4 2))

(test-case 
 "stream-mapx produces a stream"
 (check-true (stream? (stream-mapx vector distance)))
 (check-true (stream? (stream-mapx vector distance shielded)))
 (check-true (stream? (stream-mapx vector distance shielded unshielded)))
 (check-true (stream? (stream-mapx vector shielded unshielded))))

(test-case
 "stream-mapx produces correct output"
 (check-equal? (stream-first (stream-mapx vector distance)) (vector 0))
 (check-equal? (stream-first (stream-mapx vector distance shielded)) (vector 0 20))
 (check-equal? (stream-first (stream-mapx vector distance shielded unshielded)) (vector 0 20 75))
 (check-equal? (stream-first (stream-rest (stream-mapx vector distance))) (vector 5))
 (check-equal? (stream-first (stream-rest (stream-mapx vector distance shielded))) (vector 5 7))
 (check-equal? (stream-first (stream-rest (stream-mapx vector distance shielded unshielded))) (vector 5 7 25))
 (check-equal? (stream->list (stream-mapx vector unshielded shielded)) (map vector unshielded shielded)))

(test-case
 "stream-mapx works on shortest stream only"
 (check-equal? (stream-length (stream-mapx vector (in-range 4) distance)) 4)
 (check-equal? (stream-length (stream-mapx vector distance (in-range 2))) 2))

(test-case
 "streamof is true for streams of things (and also accepts lists of things)"
 (check-true ((streamof number?) (in-range 10)))
 (check-true ((streamof number?) (list 1 2 3 4 5)))
 (check-false ((streamof number?) (vector 1 2 3 4)))             
 (check-false ((streamof string?) (in-range 10))))