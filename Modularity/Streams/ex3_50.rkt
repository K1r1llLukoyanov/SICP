#lang sicp
(#%require "base.rkt")
; Exercise: finish the definition of procedure

(define (stream-map proc . argstreams)
    (if (any? null? (car argstreams))
        the-empty-stream
        (stream-cons 
            (apply proc (map stream-car argstreams))
            (apply stream-map (cons proc (map stream-cdr argstreams))))))


(define (show x)
    (display-line x)
    x)

(define (display-line x)
    (display x)
    (newline))

(define x (stream-map show (stream-enumerate-interval 0 10))) ; 0
