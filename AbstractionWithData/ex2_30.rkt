#! /usr/bin/env racket
#lang racket

; square-tree directly
(define (square-tree root)
    (cond
        ((null? root) null)
        ((not (pair? root)) (* root root))
        (else (cons (square-tree (car root)) (square-tree (cdr root))))
    ))

; square-tree using map
(define (square-tree-map tree)
    (map (lambda (tree) (* tree tree)) tree))


(define (map proc tree)
    (cond
        ((null? tree) null)
        ((not (pair? tree)) (proc tree))
        (else (cons (map proc (car tree)) (map proc (cdr tree))))
    ))


(define my-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

my-tree

(square-tree my-tree)