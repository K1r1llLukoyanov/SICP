#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (filter predicate sequence)
    (cond 
        ((null? sequence) null)
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (enumerate-interval start end)
    (if (> start end) null
    (append (list start) (enumerate-interval (+ start 1) end))))


(define (unique-pairs n)
  (accumulate append null
  (map 
	 (lambda (i)
		(map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n))))


(define (flatmap proc seq)
    (accumulate append null (map proc seq)))


(define (prime-sum? pair)
    (is-prime? (+ (car pair) (cadr pair))))


(define (is-prime? x)
    (if (> x 2) (= (smallest-divider 2 x) x) 0))


(define (smallest-divider divider number)
    (cond 
        ((not (< divider number)) number)
        ((= (remainder number divider) 0) divider)
        (else (smallest-divider (+ divider 1) number))))


(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
; result:
;'((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))


