#! /usr/bin/env racket
#lang racket


(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) null sequence))


(define (reverse2 sequence)
    (fold-left (lambda (x y) (cons x y)) null sequence))


(define (fold-left op initial sequence)
    (define (fold-left-helper op sequence result)
    (if (null? sequence)
        result
        (fold-left-helper op (cdr sequence) (op (car sequence) result))))
    (fold-left-helper op sequence initial))


(define (fold-right op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (fold-right op initial (cdr sequence)))))


(reverse (list 1 2 3 4))
(reverse2 (list 1 2 3 4))
