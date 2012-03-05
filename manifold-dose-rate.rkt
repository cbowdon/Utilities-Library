#lang racket
(require "stream-utils.rkt")

(define distance (in-range 0 35 5))

(define shielded '(20 7 3.5 2 1.5 1 1))
(define unshielded '(75 25 13 7.5 4 4 2))

(define d1 (stream-mapx vector distance shielded))
(define d2 (stream-mapx vector distance unshielded))
(define d1e (stream-mapx (lambda (x y) (vector x y (max (* 0.1 y) 1))) distance shielded))
(define d2e (stream-mapx (lambda (x y) (vector x y (max (* 0.1 y) 1))) distance unshielded))

(define (x-bound op x0 ymin ymax)
  (lambda (x) (if (op x x0) ymax ymin)))

(require plot)
(parameterize ([plot-x-label "Distance from manifold surface (cm)"]
               [plot-y-label "Dose rate (μSv/h)"]
               [plot-title "Wet Gas Meter dose rate"]
               [plot-legend-box-alpha 1]
               [plot-legend-anchor 'top-right])
  (plot (list
         (tick-grid)
         (function-interval (x-bound > 0 0 90) 
                            (x-bound > 5 0 90) 
                            #:label "Insulation thickness (dry arm only)" 
                            #:color 'gray 
                            #:alpha 0.25 )
         (lines (stream->list d1) #:label "Shielded (wet) arm" #:width 3 #:color 'red #:alpha 0.75)
         (lines (stream->list d2) #:label "Unshielded (dry) arm" #:width 3 #:color 'blue #:alpha 0.75)
         (error-bars (stream->list d1e) #:width 3 #:line-width 1 #:color 'black)       
         (error-bars (stream->list d2e) #:width 3 #:line-width 1 #:color 'black))))