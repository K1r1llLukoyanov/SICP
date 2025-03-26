#! /usr/bin/env racket
#lang racket


; Creating abstraction of rational number
; (make-rat <n> <d>) returns the rational number whose numerator is the 
; interger <n> and whose denomerator is integer <d>.
; 
; (numer <x>) returns the numerator of the rational number <x>
; (denum <x>) returns the denomerator of the rational number <x>

(define (gcd a b)
  (if (> a b)
  	(if (= (remainder a b) 0) b (gcd b (remainder a b)))
	(gcd b a)))


; arithmetics operations on rational numbers

; pair implementation
(define (cons x y)
  (define (dispatch m)
	(cond ((= m 0) x) ((= m 1) y) (else "Argument not 0 or 1 -- CONS" m)))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) 
			   (* (denom x) (numer y))) 
			(* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) 
			   (* (denom x) (numer y))) 
			(* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))


; procedures for creating and accessing rational number structures

(define (sign x)
  (cond ((= x 0) 0) ((< x 0) -1) (else 1)))

(define (make-rat n d)
  (define cdenom (gcd n d))
  (define norm (sign (* n d)))
	(if (= d 0) (error "Denominator can't be 0!")
  	(cons (* norm (/ n cdenom)) (abs (/ d cdenom)))
))


(define numer car)
(define denom cdr)



(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (writeln (denom rat)))


(define one-half (make-rat -1 2))
(define one-third (make-rat 1 3))


(print-rat (add-rat one-half one-third))