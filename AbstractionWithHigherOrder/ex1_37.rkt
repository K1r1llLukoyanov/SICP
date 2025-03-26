#! /usr/bin/env racket
#lang racket

; k-term finite continued fraction, recursive approach
(define (cont-frac-rec n d k)
    (define (frac n d rest i) (/ (n i) (+ (d i) rest)))
    (define (cont-frac i)
        (if (< i k) (frac n d (cont-frac (+ i 1)) i)
            (/ (n i) (d i))
        )
    )
    (cont-frac 1)
)

; k-term finite continued fraction, iterative approach
(define (cont-frac-iter n d k)
    (define (frac i prev-result)
        (/ (n i) (+ (d i) prev-result)))

    (define (cont-frac i result)
        (if (< i 1) result
            (cont-frac (- i 1) (frac i result))
        )
    )
    (cont-frac k 0)
)

(/ 1 (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 20))
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 20))