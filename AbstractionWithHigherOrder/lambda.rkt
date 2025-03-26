#! /usr/bin/env racket
#lang racket

(define (inc a) (+ a 1))
(define (square a) (* a a))

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
    (* dx (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)))

(define const-func (lambda (x) 5))


((lambda (x y z) (+ x y (square z))) 1 2 3)
(cubes-sum 1 4)
(- pi (* 8 (pi-sum 1 1000)))
(integral cube 0 1 0.0001)
