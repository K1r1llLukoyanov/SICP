#! /usr/bin/env racket
#lang racket


; general recursive product function
(define (product-rec term a next b)
    (if 
        (> a b) 1
        (* (term a) (product-rec term (next a) next b))
))


; general iterative product function
(define (product-iter term a next b)
    (define (product-iter-help a result)
        (if
            (> a b) result
            (product-iter-help (next a) (* result (term a)))
        )
    )
    (product-iter-help a 1)
)

(define (prod-pi a b)
    (define (term k)
        (/ (* k (+ k 2)) (* (+ k 1) (+ k 1)))
    )
    (define (next k) (+ k 2))
    (product-iter term a next b)
)

(- pi (* 4 (prod-pi 2 10000)))

