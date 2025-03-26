#! /usr/bin/env racket
#lang racket


(define empty-board null)


(define (queens board-size)
  (define (queen-cols k)
	(if (= k 0) 
   		(list empty-board)
		(filter
   			(lambda (positions) (safe? positions))
			(flatmap
				(lambda (rest-of-queens)
					(map (lambda (new-row)
							(adjoin-position new-row k rest-of-queens))
		  				(enumerate-interval 1 board-size)))
				(queens-cols (- k 1))))))
	(queen-cols board-size))


(define (enumerate-interval start end)
    (if (> start end) null
    (append (list start) (enumerate-interval (+ start 1) end))))


(define (flatmap proc seq)
    (accumulate append null (map proc seq)))


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (filter predicate sequence)
    (cond 
        ((null? sequence) null)
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (safe? positions)
  (cond ((null? positions) 1)
		((not (pair? positions)) 1)
		(else (not (accumulate or 0 (cons (check-if-attack (car positions) (cdr positions) 1)
						 	(not (safe? (cdr positions)))))))))
 

(define (check-if-attack pos rest-of-pos diff)
  (cond
	((null? rest-of-pos) 0)
	(else
  		(or (= (abs (- pos (car rest-of-pos))) diff) (check-if-atack pos (cdr rest-of-pos) (+ diff 1)))
  	)))


(define (adjoin-position new-row k rest-of-queens)
  ())



