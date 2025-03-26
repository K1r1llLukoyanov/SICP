#! /usr/bin/env racket
#lang racket

(define (fold-left op initial sequence)
    (define (fold-left-helper op sequence result)
    (if (null? sequence)
        result
        (fold-left-helper op (cdr sequence) (op (car sequence) result))
    ))
    (fold-left-helper op sequence initial))


(define (fold-right op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (fold-right op initial (cdr sequence)))
    ))


(define lst (list 1 2 3))

(fold-left / 1 lst)

(fold-left * 1 lst)
(fold-right * 1 lst)

; order of operands of operation should not affect to the result of operation