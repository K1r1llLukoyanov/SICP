#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (count-leaves t)
    (accumulate 
        (lambda (x y) (+ x y)) 
        0 
        (map 
            (lambda (x) (cond 
                ((null? x) 0)
                ((not (pair? x)) 1)
                (else (count-leaves (cdr t)))))
            t)))


(define my-tree (cons (list 1 2) (list 3 4)))


(count-leaves (cons my-tree my-tree))

