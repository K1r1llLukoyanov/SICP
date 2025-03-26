#! /usr/bin/env racket
#lang racket

; greatest common divisor by Euclid's theorem
(define (gcd a b)
    (if (= 0 (remainder a b)) b
        (gcd b (remainder a b))
    )
)


; searching for divisors
(define (smallest-divisor n)
    (find-divisor n 2))

; first first number (test-divisor) that divides n
(define (find-divisor n test-divisor)
    (cond
        ((> (square test-divisor) n) n)
        (else
            (if (divides? n test-divisor)
                test-divisor
                (find-divisor n (+ test-divisor 1))
            )
        )
    )
)

(define (square a) (* a a))

(define (divides? a b)
    (= (remainder a b) 0))


; checking if number is prime
; only divisor for prime number is 1 and number itself
(define (prime? n)
    (= (smallest-divisor n) n)
)


; The Fermat test


(gcd 72 81)
(smallest-divisor 49)
(prime? 49)
