#! /usr/bin/env racket
#lang racket

; non-optimised recursive approach
; b^n = b * b^(n-1)
; b^0 = 1
(define (expt b n)
    (if (= n 0) 1
        (* b (expt b (- n 1)))
    )
)

; iterational approach
(define (expt-iter b n)
    (define (expt-iter-helper product counter)
        (if
            (= counter n) product
            (expt-iter-helper (* b product) (+ counter 1))))
    (expt-iter-helper 1 0)            
)

; optimised recursive approach
; n%2 == 0: b^n = b^(n/2) * b^(n/2)
; else:     b^n = b^(n/2) * b^((n-1)/2 + 1)
(define (expt-opt b n)
    (cond
        ((= n 0) 1)
        ((even? n) (* (expt-iter b (/ n 2)) (expt-iter b (/ n 2))))
        (else (* b (expt-iter b (- n 1))))
    )
)

(define (my-remainder a b)
    (- (* b (floor (/ a b))) a))

(define (even? a)
    (= (my-remainder a 2) 0))


; optimised iterative approach
(define (fast-expt b n)
    (define (fast-expt-helper product counter)
        (cond
            ((= counter n) product)
            ((and (!= counter 0) (<= (* 2 counter) n)) (fast-expt-helper (* product product) (* counter 2)))
            (else (fast-expt-helper (* b product) (+ counter 1)))
        )
    )
    (fast-expt-helper 1 0)
)

(define (<= a b)
    (not (> a b)))

(define (!= a b)
    (not (= a b)))

(expt 3 4)
(expt-iter 3 4)
(expt-opt 3 4)
(fast-expt 3 4)