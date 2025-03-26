#! /usr/bin/env racket
#lang racket

(define (repeated function times)
  (if (= times 0) (lambda (x) x)
	(lambda (x) (function ((repeated function (- times 1)) x)))))

(define tolerance 0.00001)

(define (fixed-point func first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (func guess)))
   	   (if (close-enough? guess next)
		 guess
		 (try next))))
  (try first-guess))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (average x y) (/ (+ x y) 2))

(define (cube x) (* x x x))

(define (root-four x)
  (fixed-point (repeated (average-dump (lambda (y) (/ x (cube y)))) 2) 1.5))

(root-four 81)
