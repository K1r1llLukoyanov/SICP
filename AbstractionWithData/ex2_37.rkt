#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons   (accumulate op init (map (lambda (x) (car x)) seqs))
                (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))


; dot product using recursion
(define (my-dot-product v w)
    (cond
        ((and (null? v) (null? w)) 0)
        ((or (null? v) (null? w)) (error "Vectors should have equal size!"))
        (else (+ (* (car v) (car w)) (my-dot-product (cdr v) (cdr w))))))


; vector dot product using built-in map
(define (dot-product v w)
    (accumulate + 0 (map * v w)))


; multiplication of matrix and vector using recursion
(define (my-matrix-*-vector m v)
    (if (null? m) 
        null
        (append (list (my-dot-product (car m) v)) (my-matrix-*-vector (cdr m) v))
    ))


; multiplication of matrix and vector using amp
(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))


; matrix multiplication using two maps
(define (matrix-*-matrix m n)
    (let ((cols (transponse n)))
        (map (lambda (x) (map (lambda (y) (dot-product y x)) cols)) m)
    ))


; transposing matrix
(define (transponse mat)
    (accumulate-n (lambda (x y) (cons x y)) (list) mat))


(define vec1 (list 1 2 3 4))
(define vec2 (list 1 2 3 4))


(define matrix (list (list 1 1 1 1) (list 1 1 1 1) (list 1 1 1 1) (list 1 1 1 1)))
(define ones (list 1 1 1 1))


(dot-product vec1 vec2)


(matrix-*-vector matrix ones)

(matrix-*-matrix matrix matrix)

