#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (enumerate-interval start end)
  (if (> start end) null (cons start (enumerate-interval (+ start 1) end))))


(define (unique-pairs n)
  (accumulate append null
  (map 
	 (lambda (i)
		(map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n))))


(unique-pairs 6)



