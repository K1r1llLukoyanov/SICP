#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 
				0 coefficient-sequence))


(horner-eval 2 (list 1 3 0 5 0 1))

