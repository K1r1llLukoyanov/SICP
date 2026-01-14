#lang sicp

; Exercise: define mul-streams 

(define (mul-streams stream1 stream2)
    (stream-map * stream1 stream2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 1) factorials)))
; 1 2 6 24 120 720 ...
