#! /usr/bin/env racket
#lang racket

(define (sum term a next b)
    (define (sum-helper term a next b result)
        (if
            (> a b) result
            (sum-helper term (next a) next b (+ result (term a)))
        )
    )
    (sum-helper term a next b 0)
)

(define (arithm-prog a b dx)
    (define (next x) (+ x dx))
    (define (term x) x)
    (sum term a next b))


(arithm-prog 1 10 3)