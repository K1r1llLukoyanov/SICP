#! /usr/bin/env racket
#lang racket

(define (+ a b) 
    (if (= a 0) 
        b
        (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ (dec 5) 4))
; (inc (+ (- 5 1) 4))
; (inc (+ 4 4))
; (inc 8)
; 9


(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (dec b))))

; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ (- 4 1) (+ 5 1))
; (+ 3 6)
; 9