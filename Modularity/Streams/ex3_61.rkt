#lang sicp

; Exercise: write procedure to get inverse series

; X * S = 1
; S is given series
; X is reversed series
; S = n + Sr (where Sr is stream with no free coeff)
; X*(n + Sr) = 1
; X = (1 - X*Sr) / n

(define (invert-unit-series stream)
    (define inverted-stream
        (scaled-series 
            (cons-stream 1 (mul-series 
                (scaled-stream (stream-cdr stream) -1) inverted-stream))
            (/ 1 (stream-car stream))))
    inverted-stream)
