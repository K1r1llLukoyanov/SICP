#! /usr/bin/env racket
#lang racket

(define (for-each proc lst)
    (if (null? lst) #t (proc (car lst)))
    (if (null? lst) #t (for-each proc (cdr lst))))

(for-each (lambda (x) (writeln x)) (list 1 2 3 4 5))