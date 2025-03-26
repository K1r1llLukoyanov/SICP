#! /usr/bin/env racket
#lang racket

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-rect cons)
(define get-top-left car)
(define get-bottom-right cdr)

(define (rect-area rect) (* (abs (- (x-point (get-bottom-right rect)) 
									(x-point (get-top-left rect))))
							(abs (- (y-point (get-top-left rect)) 
			   						(y-point (get-bottom-right rect))))))

(define (rect-perimeter rect) (+ (* 2 (abs (- (x-point (get-bottom-right rect)) 
											  (x-point (get-top-left rect)))))
								 (* 2 (abs (- (y-point (get-top-left rect)) 
					  						(y-point (get-bottom-right rect)))))))


(define my-rect (make-rect (make-point 1 1) (make-point 4 4)))

(rect-area my-rect)
(rect-perimeter my-rect)
