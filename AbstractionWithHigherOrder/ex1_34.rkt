#! /usr/bin/env racket
#lang racket

(define (square a) (* a a))

(define (f g) (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

; (f f) => (f 2) => (2 2) => 2 is not a procedure