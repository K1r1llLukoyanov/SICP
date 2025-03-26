#! /usr/bin/env racket
#lang racket

(define (rec-fib n)
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (rec-fib (- n 1)) (rec-fib (- n 2))))
    )
)

(define (iter-fib a b count)
    (if (= count 0)
        b
    (iter-fib (+ a b) a (- count 1))))

(rec-fib 11)
(iter-fib 1 0 11)

