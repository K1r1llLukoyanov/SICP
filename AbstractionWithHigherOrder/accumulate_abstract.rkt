#! /usr/bin/env racket
#lang racket

; recursive accumulate approach
(define (accumulate-rec combiner null-value term a next b)
    (define (accumulate-rec-helper a)
        (if (> a b) null-value
            (combiner (term a) (accumulate-rec-helper (next a)))
        ))
    (accumulate-rec-helper a)
)

; iterative accumulate approach
(define (accumulate-iter combiner null-value term a next b)
    (define (accumulate-iter-helper a result)
        (if (> a b) result
            (accumulate-iter-helper (next a) (combiner (term a) result))
        )
    )
    (accumulate-iter-helper a null-value)
) 

(define (cube x) (* x x x))

(define (sum a b)
    (+ a b))

(define (product a b)
    (* a b))

(define (cubes-sum a b)
    (define (next a) (+ a 1))
    (accumulate-rec sum 0 cube a next b))

(define (factorial n)
    (define (next x) (+ x 1))
    (define (term x) x)
    (accumulate-iter product 1 term 1 next n))


; filtered accumulate, recursive approach
(define (filtered-accumulate-rec combine null-value term a next b predicate)
    (define (filtered-accumulate a)
        (if (> a b) null-value
            (if (predicate (term a)) (combine (term a) filtered-accumulate (next a))
                (filtered-accumulate (next a))
            )    
        )
    )
    (filtered-accumulate a)
)

; filtered accumulate, iterative approach
(define (filtered-accumulate-iter combine null-value term a next b predicate)
    (define (filtered-accumulate a result)
        (if (> a b) result
            (if (predicate (term a)) 
                (filtered-accumulate (next a) (combine (term a) result))
                (filtered-accumulate (next a) result)
            )
        )
    )
    (filtered-accumulate a null-value)
)

(cubes-sum 0 4)
(factorial 7)