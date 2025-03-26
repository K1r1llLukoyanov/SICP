#! /usr/bin/env racket
#lang racket

(define (rec-f n)
    (if 
        (< n 3) n
        (+ (rec-f (- n 1)) (* 2 (rec-f (- n 2))) (* 3 (rec-f (- n 3))))
    )
)

(define (iter-f n)
    (define (iter-f-count a b c count)
        (if
            (< count n) (iter-f-count (+ a (* 2 b) (* 3 c)) a b (+ count 1)) a
        )
    )
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        ((= n 3) 4)
        (else (iter-f-count 4 2 1 3))
    )
)

(define (<= a b) (not (> a b)))

(rec-f 7)
(iter-f 7)