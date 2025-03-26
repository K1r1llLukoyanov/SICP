#! /usr/bin/env racket
#lang racket

(define (fixed-point-of-trasform g transform guess)
    (fixed-point (transform g) guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next) next (try next))
        )
    )
    (try first-guess)
)

; In general, programming languages impose restrictions on the ways in which
; computational elements can be manipulated. Elements with the fewest
; restrictions are said to have first-class status.

; They may be named by variables
; They may be passed as arguments to precedures
; They may be returned as the results of precedures
; They may be included in data structures