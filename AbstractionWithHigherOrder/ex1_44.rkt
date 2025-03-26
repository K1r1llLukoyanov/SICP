#! /usr/bin/env racket
#lang racket

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated function times)
  (if (= times 0) (lambda (x) x)
	(lambda (x) (function ((repeated function (- times 1)) x)))
  )
)

(define (inc x) (+ x 1))

((repeated (smooth inc) 5) 5.0); 10
