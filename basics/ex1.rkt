#! /usr/bin/env racket
#lang racket

10; 10

(+ 5 3 4); 12

(- 9 1); 8

(/ 6 2); 3

(+ (* 2 4) (- 4 6)); 6

(define a 3); nothing (a = 3)

(define b (+ a 1)); nothing (b = 4)

(+ a b (* a b)); 19

(= a b); false (#f)

(if (and (> b a) (< b (* a b)))
    b
    a); 4

(cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25)); 16

(+ 2 (if (> b a) b a)); 6

(* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1)); 16



(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))) ; -37/150


(define (square x) 
    (* x x))

(define (min x y) 
    (if (< x y) x y))

(define (largest-sum-square x y z) 
    (- (+ (square x) (square y) (square z)) (square (min (min x y) z))))

(largest-sum-square 2 3 4)


(define ( a-plus-abs-b a b)
    ((if (< b 0) - +) a b)) ; if b < 0 then we use substract (- (-b)) = b (a + b)

(a-plus-abs-b 10 -10)