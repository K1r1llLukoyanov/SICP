#! /usr/bin/env racket
#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

((zero 3) 0)
((add-1 
