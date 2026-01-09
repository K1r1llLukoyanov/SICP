#lang sicp

; Environment is a sequance of frames
; Each frame has its link table
; Each link table consists of pairs of variables and
; its references to objects


; Procedures defined below are defined in global environment

(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (f a)
    (sum-of-squares (+ a 1) (* a 2)))

; Evaluation of (f a) creates 4 different environments
; E1 for f
; E2 for sum-of-squares
; E3 for (square x)
; E4 for (square y)

; f finds definition of sum-of-squares in global environment
; sum-of-squares finds definition of square in global environment


