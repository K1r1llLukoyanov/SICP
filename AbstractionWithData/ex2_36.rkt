#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons   (accumulate op init (map (lambda (x) (car x)) seqs))
                (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))


(define myseq (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 myseq)

