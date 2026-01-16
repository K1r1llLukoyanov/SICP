#lang sicp

; Exercise: finish the definition of procedure mul-series

(define (mul-series s1 s2)
    (cons-stream 
        (* (stream-car s1 s2)) 
        (add-streams 
            (scaled-series (stream-cdr s2) (stream-car s1))
            (mul-series (stream-cdr s1) s2))))


