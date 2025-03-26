#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) null sequence))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))


(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

