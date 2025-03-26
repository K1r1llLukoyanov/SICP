#! /usr/bin/env racket
#lang racket

(define (deep-reverse lst)
    (cond 
        ((null? lst) null)
        ((pair? lst) (append (deep-reverse (cdr lst)) (deep-reverse (car lst))))
        (else (list lst))
    ))

(define mylst (list (list 1 2) (list 3 4)))

mylst

(deep-reverse mylst)