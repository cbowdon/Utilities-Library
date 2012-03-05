#lang racket

(require "ffi-experiments.rkt")
 
(define win (initscr))
(void (waddstr win "Hello"))
(void (wrefresh win))
(sleep 1)
(void (endwin))