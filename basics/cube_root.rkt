#! /usr/bin/env racket
#lang racket

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 
    (cond 
        ((> x 10000) 0.1) 
        ((< x 1) 0.0000001)
        (else 0.001)
    )))

(define (cube-root-iter guess x)
    (if (good-enough? guess x) 
        guess
        (cube-root-iter (improve guess x)
                x)))

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root x)
    (cube-root-iter 1.0 x))

(cube-root 27)