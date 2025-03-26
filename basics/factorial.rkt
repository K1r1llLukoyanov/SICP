#! /usr/bin/env racket
#lang racket

; recurisive approach
(define (rec-fact n)
    (if (< n 2) 1 (* n (rec-fact (- n 1)))))


; iterarive approach
(define (iter-fact n)
    (define (iter product counter)
        (if 
            (> counter n) product 
            (iter (* product counter) (+ counter 1))
        )
    )
    (iter 1 1)
)

(iter-fact 6)
(rec-fact 6)