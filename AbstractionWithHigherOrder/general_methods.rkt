#! /usr/bin/env racket
#lang racket

; finding roots of equations by the half-interval method
; we need to find such x so f(x) = 0
; we suppose that x is between two points "a" and "b"
; and f(a)*f(b) <= 0
; then we take central point "c" between these points, so c = a + (b-a)/2
; and then we check if this point is good enough by abs(f(c)) < 0
; if this statement is true then we return "c"
; otherwise, we continue by replacing point "a" or "b" with point "c"
; depending on sign of f(c)

(define (average a b) (/ (+ a b) 2))
(define (abs x) (if (< x 0) (- 0 x) x))
(define (close-enough? a b) (< (abs (- a b)) 0.001))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))


; half-interval method
(define (search f neg-point pos-point)
    (let 
        ((mid-point (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point) 
            mid-point
            (let ((test-value (f mid-point)))
                (cond
                    ((positive? test-value) (search f neg-point mid-point))
                    ((negative? test-value) (search f mid-point pos-point))
                    (else mid-point))))))

(define (half-interval-method f a b)
    (let
        (
            (a-value (f a))
            (b-value (f b))
        )
        (cond
            ((and (negative? a-value) (positive? b-value)) (search f a b))
            ((and (positive? a-value) (negative? b-value)) (search f b a))
            (else (error "Values are not of opposite sign" a-value b-value)))))


(define (func x) x)
(half-interval-method sin 2.0 4.0); 0
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


; finding fixed points of functions
; A number x is called a fixed point of function f if x satisfies 
; the equation f(x) = x.
; 
; For some function f we can locate a fixed point by beginning with an initial guess 
; and applying f repeatedy,
; f(x), f(f(x)), f(f(f(x))), ...

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next) next (try next))
        )
    )
    (try first-guess)
)

(define (square x) (* x x))
(fixed-point sin 0.5)



