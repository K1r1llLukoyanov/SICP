#! /usr/bin/env racket
#lang racket


(define (mult a b)
    (define (mult-helper result counter)
        (cond
            ((= counter b) result)
            ((and (not (= counter 0)) (< (* 2 counter) b)) (mult-helper (double result) (double counter)))
            (else (mult-helper (+ a result) (+ counter 1)))
        )
    )
    (mult-helper 0 0)
)


(define (fast-mult a b)
    (cond
        ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))
    )
)


(define (double a)
    (+ a a))


(define (halve a)
    (/ a 2))


(define (even? a)
    (= (remainder a 2) 0))


(mult 100 100)
(fast-mult 7 9)

