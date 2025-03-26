#! /usr/bin/env racket
#lang racket

; interval structure representation
; specifying lower bound and upper bound values
(define (make-interval lower upper) (cons lower upper))

(define (lower-bound interval) (car interval)) 

(define (upper-bound interval) (cdr interval))

(define (interval-width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))


; making interval
; specifying central point and interval width
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; making interval
; specifying midpoint and percentage tolerance

(define (make-center-percent mid perc)
  (let ((width (* mid (/ perc 100))))
	(make-interval (- mid width) (+ mid width))))

; getting percent tolerance
(define (percent i)
  (* 100 (/ (width i) (center i))))

; arithmetics actions on interval values
(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y)) 
				 (+ (upper-bound y) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y)) 
				 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval  (min p1 p2 p3 p4)
					(max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0)) (error "Lower or upper bound of interval can't be zero!")
	(mul-interval x (make-interval (/ 1.0 (upper-bound y))
								 (/ 1.0 (lower-bound y))))))


