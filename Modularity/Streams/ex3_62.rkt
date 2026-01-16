#lang sicp

; Exercise: define div-series procedure which work with any two series

(define (invert-unit-series stream)
    (define inverted-stream
        (scaled-series 
            (cons-stream 1 (mul-series
                (scaled-stream (stream-cdr stream) -1) inverted-stream))
            (/ 1 (stream-car stream))))
    inverted-stream)

(define (div-series s1 s2)
    (if (= (stream-car s2) 0)
        (error "Denomerator should have 0 as free coeff" s2)
        (mul-streams s1 (invert-unit-series s2))))

(define tan-series (div-series sine-stream cosine-stream))
