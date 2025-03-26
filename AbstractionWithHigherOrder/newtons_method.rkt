#! /usr/bin/env racket
#lang racket

(define (square x) (* x x))

(define (cube x) (* x x x))

(define dx 0.00001)

(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x)) dx)
    ))


(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))
    ))

(define (newton-method g guess)
    (fixed-point (newton-transform g) guess))

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
    (newton-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 4)





