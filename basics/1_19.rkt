#! /usr/bin/env racket
#lang racket

(define (fib n)
    (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
    (cond
        ((= count 0) b)
        ((even? count) 
            (fib-iter
                a 
                b 
                (+ (* q q) (* p p))
                (+ (* q q) (* 2 p))
                (/ count 2)
            ))
        (else 
            (fib-iter 
                (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p
                q
                (- count 1)
            ))))

(fib 5)
(fib 6)
(fib 7)

