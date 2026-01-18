#lang sicp

; Exercise: Write program to simulate RC-circuit

(define (integrate integrand init-value dt)
    (define int
        (cons-stream init-value
            (sum-streams int (scaled-stream integrand dt))))
    int)

(define (RC R C dt)
    (lambda (I V0)
        (sum-streams 
            (scaled-stream I R)
            (integrate (scaled-stream I (/ 1.0 C)) V0 dt))))
