#! /usr/bin/env racket
#lang racket

(define (iterative-improve good-enough? improvement)
  (lambda (guess) (let ((next (improvement guess)))
	(if (good-enough? guess next) 
   	  guess
	  ((iterative-improve good-enough? improvement) (improvement guess))))))

(define (close-enough? x0 x1) (< (abs (- x0 x1)) 0.00001))
