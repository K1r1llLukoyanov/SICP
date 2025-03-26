#! /usr/bin/env racket
#lang racket

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (writeln guess)
        (let ((next (f guess)))
            (if (close-enough? guess next) guess
                (try next)
            )
        )
    )
    (try first-guess)
)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)