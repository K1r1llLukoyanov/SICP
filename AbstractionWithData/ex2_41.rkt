#! /usr/bin/env racket
#lang racket


(define (accumulate op init lst)
    (if (null? lst) init (op (car lst) (accumulate op init (cdr lst)))))


(define (enumerate-interval start end)
  (if (> start end) null (cons start (enumerate-interval (+ start 1) end))))


(define (unique-triplets n)
  (accumulate append null
	(map 
		(lambda (i) 
			(accumulate append null 
				(map (lambda (j) 
		   			(map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1))))
					   (enumerate-interval 2 (- i 1))))) (enumerate-interval 3 n))))


(define (filter predicate sequence)
  (cond ((null? sequence) null)
		((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))


(define (sum-triplets n s)
  (filter (lambda (triplet) (equals-sum triplet s)) (unique-triplets n)))


(define (equals-sum triplet s)
  (if (null? triplet) 0
  	(= (+ (car triplet) (car (cdr triplet)) (car (cdr (cdr triplet)))) s)))


(sum-triplets 6 9)
