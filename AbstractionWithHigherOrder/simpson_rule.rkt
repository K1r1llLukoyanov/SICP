#! /usr/bin/env racket
#lang racket

; Simpson rule for numerical integration:
; where h = (b - a)/n
; y_k = f(a + kh)
; integral of f = h*(y_0 + 4*y_1 + 2*y_2 + 4*y_3 + 2*y_4 + ... + 2*y_(n-2) + 4*y_(n-1) + y_n)

(define (cube x) (* x x x))

(define (sum term a next b)
    (if
        (> a b) 0
        (+ (term a) (sum term (next a) next b))
    )
)

(define (simpson-int f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* k h))))
    (define (term k)
        (cond
            ((or (= k 0) (= k n)) (y k))
            ((= (remainder k 2) 0) (* 2 (y k)))
            (else
                (* 4 (y k)))))
    (define (simpson-sum k)
        (if
            (> k n) 0
            (+ (term k) (simpson-sum (+ k 1)))))

    (/ (* h (simpson-sum 0)) 3)
)


(define (rect-int f a b dx)
    (define (next x) (+ x dx))
    (* dx (sum f (+ a (/ dx 2)) next b))
)

(rect-int cube 0 1 0.0001)
(simpson-int cube 0 1 10000)