#! /usr/bin/env racket
#lang racket

(define (repeated function times)
  (if (= times 0) (lambda (x) x)
	(lambda (x) (function ((repeated function (- times 1)) x)))
  )
)

(define (square x) (* x x))

((repeated square 2) 5.0)
