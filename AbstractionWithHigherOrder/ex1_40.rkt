#! /usr/bin/env racket
#lang racket

(define dx 0.00001)
(define tolerance 0.000001)

(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newtons-method function start)
  (fixed-point (lambda (x) (- x (/ (function x) ((deriv function) x)))) start))

(define (fixed-point function first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (function guess)))
   		(if (close-enough? guess next) guess
	  		(try next)
		)
	))
  (try first-guess)
)


(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (square x) (* x x))
(define (cube x) (* x x x))

(newtons-method (cubic 1 1 1) 0.6)
