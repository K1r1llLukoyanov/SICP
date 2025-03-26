#! /usr/bin/env racket
#lang racket

(define (square a) (* a a))

; example:
; calculation of function: f(x, y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)

; approach with local bindings
(define (f x y)
    (define (f-helper a b)
        (+  (* x (square a))
            (* y b)
            (* a b)))
    (f-helper   (+ 1 (* x y)) 
                (- 1 y)))


; approach with lambdas
(define (f-lambdas x y)
    ((lambda (a b)
        (+  (* x (square a))
            (* y b)
            (* a b)
        )
    )
    (+ 1 (* x y))
    (- 1 y)))


; using "let"

(define (f-let x y)
    (let 
        (
            (a (+ 1 (* x y)))
            (b (- 1 y))
        )
        (+  
            (* x (square a))
            (* y b)
            (* a b)
        )
    )
)

(let 
    ((x 10))    ; each x has it's own scope
    (+ (let ((x 3))
    (+ x (* x 10)) ; inner x has been used
    ) x) ; outer x has been used
); result is 43

