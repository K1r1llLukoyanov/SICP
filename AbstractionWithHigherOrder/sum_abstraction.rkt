#! /usr/bin/env racket
#lang racket

(define (inc a) (+ a 1))

; very general function for sum calculation
; sum = f(a) + f(next(a)) + f(next(next a)) + ... + f(b)
; where f is (term a) function
(define (sum term a next b)
    (if (> a b) 0
        (+ (term a) (sum term (next a) next b))))

(define (cube a) (* a a a))

; sum of cubes: a^3 + (a+1)^3 + ... + b^3
(define (cubes-sum a b)
    (sum cube a inc b))

; pi/8 = 1/(1*3) + 1/(5*7) + ... + 1/(n * (n + 2))

(define (pi-sum a b)
    (sum 
        (lambda (x) (/ 1 (* x (+ x 2)))) 
        a 
        (lambda (x) (+ x 4))
        b
    )
)


; finding integral
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (const-func x) 5)

(cubes-sum 1 4)
(- pi (* 8 (pi-sum 1 1000)))
(integral cube 0 1 0.0001)