#! /usr/bin/env racket
#lang racket

(define (power a n) (if (= n 0) 1 (* a (power a (- n 1)))))

(define (cons a b) (* (power 2 a) (power 3 b)))

(define (car z)
  (if (= (remainder z 3) 0) (car (/ z 3)) (/ (log z) (log 2))))

(define (cdr z)
  (if (= (remainder z 2) 0) (cdr (/ z 2)) (/ (log z) (log 3))))

(car (cons 5 5)); 5
(cdr (cons 5 7)); 7
