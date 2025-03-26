#! /usr/bin/env racket
#lang racket



(define (square-tree tree)
    (tree-map square tree))


(define (tree-map proc tree)
    (cond
        ((null? tree) null)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
    ))


(define (square x) (* x x))


(define my-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

my-tree

(square-tree my-tree)