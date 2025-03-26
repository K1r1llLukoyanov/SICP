#! /usr/bin/env racket
#lang racket

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

; average-dump is a function that take a function as argument
; and return another function
(define (average-dump f)
    (lambda (x) (average x (f x))))

((average-dump square) 10); 55


; reformulating square root function with average-dump
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next) guess
                (try next)
            )
        )
    )
    (try first-guess)
)

(define (sqrt x)
    (fixed-point (average-dump (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
    (fixed-point (average-dump (lambda (y) (/ x (square y)))) 1.0)
)

(sqrt 4)        ; 2
(cube-root 27)  ; 3